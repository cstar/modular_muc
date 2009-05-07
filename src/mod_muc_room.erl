%%%----------------------------------------------------------------------
%%% File    : mod_muc_room.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : MUC room stuff
%%% Created : 19 Mar 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2009   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------
%%@hidden
-module(mod_muc_room).
-author('alexey@process-one.net').

-behaviour(gen_fsm).


%% External exports
-export([start_link/11,
	 start_link/9,
	 start/11,
	 start/9,
	 route/4]).

%% gen_fsm callbacks
-export([init/1,
	 normal_state/2,
	 handle_event/3,
	 handle_sync_event/4,
	 handle_info/3,
	 terminate/3,
	 code_change/4]).

%% For muc_room_handler
-export([
     add_to_log/3,
     get_affiliation/2,
     find_jid_by_nick/2,
     destroy_room/2,
     get_role/2,
     set_role/3,
     get_service_affiliation/2,
     set_affiliation_and_reason/4,
     set_affiliation/3,
     get_service_max_users/1,
     get_max_users/1,
     send_update_presence/3,
     send_new_presence/3,
     send_kickban_presence/4,
     remove_nonmembers/1,
     send_update_presence/2,
     check_subject/1,
     extract_password/1,
     strip_status/1]).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_muc_room.hrl").

-define(DEFAULT_HANDLER, minimal_muc).

-define(DBGFSM, true).

-ifdef(DBGFSM).
-define(FSMOPTS, [{debug, [trace]}]).
-else.
-define(FSMOPTS, []).
-endif.

%% Module start with or without supervisor:
-ifdef(NO_TRANSIENT_SUPERVISORS).
-define(SUPERVISOR_START, 
	gen_fsm:start(?MODULE, [Host, ServerHost, Access, Room, HistorySize,
				RoomShaper, Creator, Nick, DefRoomOpts, Handler, Storage],
		      ?FSMOPTS)).
-else.
-define(SUPERVISOR_START, 
	Supervisor = gen_mod:get_module_proc(ServerHost, ejabberd_mod_muc_sup),
	supervisor:start_child(
	  Supervisor, [Host, ServerHost, Access, Room, HistorySize, RoomShaper,
		       Creator, Nick, DefRoomOpts, Handler, Storage])).
-endif.

%%%----------------------------------------------------------------------
%%% API
%%%----------------------------------------------------------------------
start(Host, ServerHost, Access, Room, HistorySize, RoomShaper,
      Creator, Nick, DefRoomOpts, Handler, Storage) ->
    ?SUPERVISOR_START.

start(Host, ServerHost, Access, Room, HistorySize, RoomShaper, Opts, Handler, Storage) ->
    Supervisor = gen_mod:get_module_proc(ServerHost, ejabberd_mod_muc_sup),
    supervisor:start_child(
      Supervisor, [Host, ServerHost, Access, Room, HistorySize, RoomShaper,
		   Opts, Handler, Storage]).

start_link(Host, ServerHost, Access, Room, HistorySize, RoomShaper,
	   Creator, Nick, DefRoomOpts, Handler, Storage) ->
    gen_fsm:start_link(?MODULE, [Host, ServerHost, Access, Room, HistorySize,
				 RoomShaper, Creator, Nick, DefRoomOpts, Handler, Storage],
		       ?FSMOPTS).

start_link(Host, ServerHost, Access, Room, HistorySize, RoomShaper, Opts, Handler, Storage) ->
    gen_fsm:start_link(?MODULE, [Host, ServerHost, Access, Room, HistorySize,
				 RoomShaper, Opts, Handler, Storage],
		       ?FSMOPTS).

%%%----------------------------------------------------------------------
%%% Callback functions from gen_fsm
%%%----------------------------------------------------------------------

%%----------------------------------------------------------------------
%% Func: init/1
%% Returns: {ok, StateName, StateData}          |
%%          {ok, StateName, StateData, Timeout} |
%%          ignore                              |
%%          {stop, StopReason}
%%----------------------------------------------------------------------
init([Host, ServerHost, Access, Room, HistorySize, RoomShaper, Creator, Nick, DefRoomOpts, Handler, Storage]) ->
    process_flag(trap_exit, true),
    Shaper = shaper:new(RoomShaper),
    State = #state{host = Host,
				   server_host = ServerHost,
				   access = Access,
				   room = Room,
				   storage = Storage,
				   handler = Handler,
				   history = lqueue_new(HistorySize),
				   jid = jlib:make_jid(Room, Host, ""),
				   just_created = true,
				   room_shaper = Shaper},
    case handler_call(init, [DefRoomOpts,Creator, Nick], State) of
        {result, ok, State1}->
            ?INFO_MSG("Created MUC room ~s@~s by ~s", 
	            [Room, Host, jlib:jid_to_string(Creator)]),
	        {ok, normal_state, State1};
        {error, Why, _State1}->
            ?ERROR_MSG("Handler ~p returned an error for ~p : ~p",[Handler, Room, Why]),
            {stop, Why}
    end;
    
init([Host, ServerHost, Access, Room, HistorySize, RoomShaper, Opts, Handler, Storage]) ->
    process_flag(trap_exit, true),
    Shaper = shaper:new(RoomShaper),
    State = #state{host = Host,
				  server_host = ServerHost,
				  access = Access,
				  room = Room,
				  storage = Storage,
				  handler=Handler,
				  history = lqueue_new(HistorySize),
				  jid = jlib:make_jid(Room, Host, ""),
				  room_shaper = Shaper},
    case handler_call(init, [Opts], State) of
        {result, ok, State1}->
            ?INFO_MSG("Fetched MUC room ~s@~s ", 
	            [Room, Host]),
	        {ok, normal_state, State1};
        {error, Why, _State1}->
            ?ERROR_MSG("Handler ~p returned an error for ~p : ~p",[Handler, Room, Why]),
            {stop, Why}
    end.

%%----------------------------------------------------------------------
%% Func: StateName/2
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
normal_state({route, From, "",
	      {xmlelement, "message", Attrs, Els} = Packet},
	     StateData) ->
    Lang = xml:get_attr_s("xml:lang", Attrs),
    case is_user_online(From, StateData) orelse
	is_user_allowed_message_nonparticipant(From, StateData) of
	true ->
	    case xml:get_attr_s("type", Attrs) of
		"groupchat" ->
		    Activity = get_user_activity(From, StateData),
		    Now = now_to_usec(now()),
		    MinMessageInterval =
			trunc(gen_mod:get_module_opt(
				StateData#state.server_host,
				mod_muc, min_message_interval, 0) * 1000000),
		    Size = lists:flatlength(xml:element_to_string(Packet)),
		    {MessageShaper, MessageShaperInterval} =
			shaper:update(Activity#activity.message_shaper, Size),
		    if
			Activity#activity.message /= undefined ->
			    ErrText = "Traffic rate limit is exceeded",
			    Err = jlib:make_error_reply(
				    Packet, ?ERRT_RESOURCE_CONSTRAINT(Lang, ErrText)),
			    ejabberd_router:route(
			      StateData#state.jid,
			      From, Err),
			    {next_state, normal_state, StateData};
			Now >= Activity#activity.message_time + MinMessageInterval,
			MessageShaperInterval == 0 ->
			    {RoomShaper, RoomShaperInterval} =
				shaper:update(StateData#state.room_shaper, Size),
			    RoomQueueEmpty = queue:is_empty(
					       StateData#state.room_queue),
			    if
				RoomShaperInterval == 0,
				RoomQueueEmpty ->
				    NewActivity = Activity#activity{
						    message_time = Now,
						    message_shaper = MessageShaper},
				    StateData1 =
					store_user_activity(
					  From, NewActivity, StateData),
				    StateData2 =
					StateData1#state{
					  room_shaper = RoomShaper},
				    process_groupchat_message(From, Packet, StateData2);
				true ->
				    StateData1 =
					if
					    RoomQueueEmpty ->
						erlang:send_after(
						  RoomShaperInterval, self(),
						  process_room_queue),
						StateData#state{
						  room_shaper = RoomShaper};
					    true ->
						StateData
					end,
				    NewActivity = Activity#activity{
						    message_time = Now,
						    message_shaper = MessageShaper,
						    message = Packet},
				    RoomQueue = queue:in(
						  {message, From},
						  StateData#state.room_queue),
				    StateData2 =
					store_user_activity(
					  From, NewActivity, StateData1),
				    StateData3 =
					StateData2#state{
					  room_queue = RoomQueue},
				    {next_state, normal_state, StateData3}
			    end;
			true ->
			    MessageInterval =
				(Activity#activity.message_time +
				 MinMessageInterval - Now) div 1000,
			    Interval = lists:max([MessageInterval,
						  MessageShaperInterval]),
			    erlang:send_after(
			      Interval, self(), {process_user_message, From}),
			    NewActivity = Activity#activity{
					    message = Packet,
					    message_shaper = MessageShaper},
			    StateData1 =
				store_user_activity(
				  From, NewActivity, StateData),
			    {next_state, normal_state, StateData1}
		    end;
		"error" ->
		    case is_user_online(From, StateData) of
			true ->
			    ErrorText = "This participant is kicked from the room because "
				"he sent an error message",
			    NewState = expulse_participant(Packet, From, StateData, 
					 translate:translate(Lang, ErrorText)),
			    {next_state, normal_state, NewState};
			_ ->
			    {next_state, normal_state, StateData}
		    end;
		"chat" ->
		    ErrText = "It is not allowed to send private messages to the conference",
		    Err = jlib:make_error_reply(
			    Packet, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)),
		    ejabberd_router:route(
		      StateData#state.jid,
		      From, Err),
		    {next_state, normal_state, StateData};
		Type when (Type == "") or (Type == "normal") ->
		    case catch check_invitation(From, Els, Lang, StateData) of
			{error, Error} ->
			    Err = jlib:make_error_reply(
				    Packet, Error),
			    ejabberd_router:route(
			      StateData#state.jid,
			      From, Err),
			    {next_state, normal_state, StateData};
			NSD ->
			    {next_state, normal_state, NSD}
		    end;
		_ ->
		    ErrText = "Improper message type",
		    Err = jlib:make_error_reply(
			    Packet, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)),
		    ejabberd_router:route(
		      StateData#state.jid,
		      From, Err),
		    {next_state, normal_state, StateData}
	    end;
	_ ->
	    case xml:get_attr_s("type", Attrs) of
		"error" ->
		    ok;
		_ ->
		    handle_roommessage_from_nonparticipant(Packet, Lang, StateData, From)
	    end,
	    {next_state, normal_state, StateData}
    end;

normal_state({route, From, "",
	      {xmlelement, "iq", _Attrs, _Els} = Packet},
	     StateData) ->
    case jlib:iq_query_info(Packet) of
	#iq{type = Type, xmlns = XMLNS, lang = Lang, sub_el = SubEl} = IQ  ->
        UserInfo = get_user_info(From, StateData),
        UAffiliation = get_affiliation(From, StateData),
	    Res1 = process_iq(UserInfo,UAffiliation, XMLNS, Type, Lang, SubEl, StateData),
	    {IQRes, NewStateData} =
		case Res1 of
		    {result, Res, SD} ->
			{IQ#iq{type = result,
			       sub_el = [{xmlelement, "query",
					  [{"xmlns", XMLNS}],
					  Res
					 }]},
			 SD};
		    {error, Error, SD} ->
			{IQ#iq{type = error,
			       sub_el = [SubEl, Error]},
			 SD};
			 {error, Error} -> 
			 {IQ#iq{type = error,
			       sub_el = [SubEl, Error]},
			 StateData}
		end,
	    ejabberd_router:route(StateData#state.jid,
				  From,
				  jlib:iq_to_xml(IQRes)),
		%%TODO
	    case NewStateData of
		stop ->
		    {stop, normal, StateData};
		_ ->
		    {next_state, normal_state, NewStateData}
	    end;
	reply ->
	    {next_state, normal_state, StateData};
	_ ->
	    Err = jlib:make_error_reply(
		    Packet, ?ERR_FEATURE_NOT_IMPLEMENTED),
	    ejabberd_router:route(StateData#state.jid, From, Err),
	    {next_state, normal_state, StateData}
    end;

normal_state({route, From, Nick,
	      {xmlelement, "presence", _Attrs, _Els} = Packet},
	     StateData) ->
    Activity = get_user_activity(From, StateData),
    Now = now_to_usec(now()),
    MinPresenceInterval =
	trunc(gen_mod:get_module_opt(
		StateData#state.server_host,
		mod_muc, min_presence_interval, 0) * 1000000),
    if
	(Now >= Activity#activity.presence_time + MinPresenceInterval) and
	(Activity#activity.presence == undefined) ->
	    NewActivity = Activity#activity{presence_time = Now},
	    StateData1 = store_user_activity(From, NewActivity, StateData),
	    process_presence(From, Nick, Packet, StateData1);
	true ->
	    if
		Activity#activity.presence == undefined ->
		    Interval = (Activity#activity.presence_time +
				MinPresenceInterval - Now) div 1000,
		    erlang:send_after(
		      Interval, self(), {process_user_presence, From});
		true ->
		    ok
	    end,
	    NewActivity = Activity#activity{presence = {Nick, Packet}},
	    StateData1 = store_user_activity(From, NewActivity, StateData),
	    {next_state, normal_state, StateData1}
    end;

normal_state({route, From, ToNick,
	      {xmlelement, "message", Attrs, _} = Packet},
	     StateData) ->
    Type = xml:get_attr_s("type", Attrs),
    Lang = xml:get_attr_s("xml:lang", Attrs),
    case decide_fate_message(Type, Packet, From, StateData) of
	{expulse_sender, _Reason} ->
	    ErrorText = "This participant is kicked from the room because "
		"he sent an error message to another participant",
	    NewState = expulse_participant(Packet, From, StateData, 
					   translate:translate(Lang, ErrorText)),
	    {next_state, normal_state, NewState};
	forget_message ->
	    {next_state, normal_state, StateData};
	continue_delivery ->
	    NewState = case {handler_call(process_private_message, [From, ToNick, Packet], StateData), is_user_online(From, StateData)} of
		{{allow, NewPacket, NSD}, true} ->
		    case Type of
			"groupchat" ->
			    ErrText = "It is not allowed to send private "
				"messages of type \"groupchat\"",
			    Err = jlib:make_error_reply(
				    Packet, ?ERRT_BAD_REQUEST(Lang, ErrText)),
			    ejabberd_router:route(
			      jlib:jid_replace_resource(
				NSD#state.jid,
				ToNick),
			      From, Err),
			      NSD;
			_ ->
			    case find_jid_by_nick(ToNick, NSD) of
				false ->
				    ErrText = "Recipient is not in the conference room",
				    Err = jlib:make_error_reply(
					    Packet, ?ERRT_ITEM_NOT_FOUND(Lang, ErrText)),
				    ejabberd_router:route(
				      jlib:jid_replace_resource(
					NSD#state.jid,
					ToNick),
				      From, Err),
				      NSD;
				ToJID ->
				    {ok, #user{nick = FromNick}} =
					?DICT:find(jlib:jid_tolower(From),
						   NSD#state.users),
				    ejabberd_router:route(
				      jlib:jid_replace_resource(
					NSD#state.jid,
					FromNick),
				      ToJID, NewPacket),
				    NSD
			    end
		    end;
		{{allow, _, NSD}, false} ->
		    ErrText = "Only occupants are allowed to send messages to the conference",
		    Err = jlib:make_error_reply(
			    Packet, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)),
		    ejabberd_router:route(
		      jlib:jid_replace_resource(
			NSD#state.jid,
			ToNick),
		      From, Err),
		    NSD;
		{{deny, ErrText, NSD}, _} ->
		    Err = jlib:make_error_reply(
			    Packet, ?ERRT_FORBIDDEN(Lang, ErrText)),
		    ejabberd_router:route(
		      jlib:jid_replace_resource(
			NSD#state.jid,
			ToNick),
		      From, Err),
		    NSD
	    end,
	    {next_state, normal_state, NewState}
    end;

normal_state({route, From, ToNick,
	      {xmlelement, "iq", Attrs, _Els} = Packet},
	     StateData) ->
    Lang = xml:get_attr_s("xml:lang", Attrs),
    %TODO
    NewState = case {handler_call(process_user_iq, [From, ToNick, Lang, Packet],StateData),
	  is_user_online(From, StateData)} of
	{{allow, P2, NSD}, true} ->
	    case find_jid_by_nick(ToNick, NSD) of
		false ->
		    case jlib:iq_query_info(Packet) of
			reply ->
			    NSD;
			_ ->
			    ErrText = "Recipient is not in the conference room",
			    Err = jlib:make_error_reply(
				    Packet, ?ERRT_ITEM_NOT_FOUND(Lang, ErrText)),
			    ejabberd_router:route(
			      jlib:jid_replace_resource(
				NSD#state.jid, ToNick),
			      From, Err),
			    NSD
		    end;
		ToJID ->
		    {ok, #user{nick = FromNick}} =
			?DICT:find(jlib:jid_tolower(From),
				   NSD#state.users),
		    ejabberd_router:route(
		      jlib:jid_replace_resource(NSD#state.jid, FromNick),
		      ToJID, P2),
		    NSD
	    end;
	{{_, _, NSD}, false} ->
	    case jlib:iq_query_info(Packet) of
		reply ->
		    NSD;
		_ ->
		    ErrText = "Only occupants are allowed to send queries to the conference",
		    Err = jlib:make_error_reply(
			    Packet, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)),
		    ejabberd_router:route(
		      jlib:jid_replace_resource(NSD#state.jid, ToNick),
		      From, Err),
		    NSD
	    end;
	{{_, _, NSD}, _} ->
	    case jlib:iq_query_info(Packet) of
		reply ->
		    NSD;
		_ ->
		    ErrText = "Queries to the conference members are not allowed in this room",
		    Err = jlib:make_error_reply(
			    Packet, ?ERRT_NOT_ALLOWED(Lang, ErrText)),
		    ejabberd_router:route(
		      jlib:jid_replace_resource(NSD#state.jid, ToNick),
		      From, Err),
		    NSD
	    end
    end,
    {next_state, normal_state, NewState};

normal_state(_Event, StateData) ->
    {next_state, normal_state, StateData}.



%%----------------------------------------------------------------------
%% Func: handle_event/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_event({service_message, Msg}, _StateName, StateData) ->
    MessagePkt = {xmlelement, "message",
		  [{"type", "groupchat"}],
		  [{xmlelement, "body", [], [{xmlcdata, Msg}]}]},
    lists:foreach(
      fun({_LJID, Info}) ->
	      ejabberd_router:route(
		StateData#state.jid,
		Info#user.jid,
		MessagePkt)
      end,
      ?DICT:to_list(StateData#state.users)),
    NSD = add_message_to_history("",
				 MessagePkt,
				 StateData),
    {next_state, normal_state, NSD};

handle_event({destroy, Reason}, _StateName, StateData) ->
    {result, [], stop} =
        destroy_room(
          {xmlelement, "destroy",
           [{"xmlns", ?NS_MUC_OWNER}],
           case Reason of
               none -> [];
               _Else ->
                   [{xmlelement, "reason",
                     [], [{xmlcdata, Reason}]}]
           end}, StateData),
    ?INFO_MSG("Destroyed MUC room ~s with reason: ~p", 
	      [jlib:jid_to_string(StateData#state.jid), Reason]),
    {stop, normal, StateData};
handle_event(destroy, StateName, StateData) ->
    ?INFO_MSG("Destroyed MUC room ~s", 
	      [jlib:jid_to_string(StateData#state.jid)]),
    handle_event({destroy, none}, StateName, StateData);

handle_event({set_affiliations, Affiliations}, StateName, StateData) ->
    {next_state, StateName, StateData#state{affiliations = Affiliations}};

handle_event(_Event, StateName, StateData) ->
    {next_state, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_sync_event/4
%% Returns: {next_state, NextStateName, NextStateData}            |
%%          {next_state, NextStateName, NextStateData, Timeout}   |
%%          {reply, Reply, NextStateName, NextStateData}          |
%%          {reply, Reply, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}                          |
%%          {stop, Reason, Reply, NewStateData}
%%----------------------------------------------------------------------
  
handle_sync_event(get_config, _From, StateName, StateData) ->
    {reply, {ok, StateData#state.config}, StateName, StateData};
handle_sync_event(get_state, _From, StateName, StateData) ->
    {reply, {ok, StateData}, StateName, StateData};
%handle_sync_event({change_config, Config}, _From, StateName, #state{handler=Handler}=StateData) ->
%    {result, [], NSD} = Handler:change_config(Config, StateData),
%    {reply, {ok, NSD#state.config}, StateName, NSD};
handle_sync_event({change_state, NewStateData}, _From, StateName, _StateData) ->
    {reply, {ok, NewStateData}, StateName, NewStateData};
handle_sync_event(Event, From, StateName, StateData) ->
    {Return,Values,NewState} = handler_call(handle_sync_event, [Event, From], StateData),
    {reply, {Return, Values}, StateName, NewState}.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%----------------------------------------------------------------------
%% Func: handle_info/3
%% Returns: {next_state, NextStateName, NextStateData}          |
%%          {next_state, NextStateName, NextStateData, Timeout} |
%%          {stop, Reason, NewStateData}
%%----------------------------------------------------------------------
handle_info({process_user_presence, From}, normal_state = _StateName, StateData) ->
    RoomQueueEmpty = queue:is_empty(StateData#state.room_queue),
    RoomQueue = queue:in({presence, From}, StateData#state.room_queue),
    StateData1 = StateData#state{room_queue = RoomQueue},
    if
	RoomQueueEmpty ->
	    StateData2 = prepare_room_queue(StateData1),
	    {next_state, normal_state, StateData2};
	true ->
	    {next_state, normal_state, StateData1}
    end;
handle_info({process_user_message, From}, normal_state = _StateName, StateData) ->
    RoomQueueEmpty = queue:is_empty(StateData#state.room_queue),
    RoomQueue = queue:in({message, From}, StateData#state.room_queue),
    StateData1 = StateData#state{room_queue = RoomQueue},
    if
	RoomQueueEmpty ->
	    StateData2 = prepare_room_queue(StateData1),
	    {next_state, normal_state, StateData2};
	true ->
	    {next_state, normal_state, StateData1}
    end;
handle_info(process_room_queue, normal_state = StateName, StateData) ->
    case queue:out(StateData#state.room_queue) of
	{{value, {message, From}}, RoomQueue} ->
	    Activity = get_user_activity(From, StateData),
	    Packet = Activity#activity.message,
	    NewActivity = Activity#activity{message = undefined},
	    StateData1 =
		store_user_activity(
		  From, NewActivity, StateData),
	    StateData2 =
		StateData1#state{
		  room_queue = RoomQueue},
	    StateData3 = prepare_room_queue(StateData2),
	    process_groupchat_message(From, Packet, StateData3);
	{{value, {presence, From}}, RoomQueue} ->
	    Activity = get_user_activity(From, StateData),
	    {Nick, Packet} = Activity#activity.presence,
	    NewActivity = Activity#activity{presence = undefined},
	    StateData1 =
		store_user_activity(
		  From, NewActivity, StateData),
	    StateData2 =
		StateData1#state{
		  room_queue = RoomQueue},
	    StateData3 = prepare_room_queue(StateData2),
	    process_presence(From, Nick, Packet, StateData3);
	{empty, _} ->
	    {next_state, StateName, StateData}
    end;
handle_info({captcha_succeed, From}, normal_state, StateData) ->
    NewState = case ?DICT:find(From, StateData#state.robots) of
		   {ok, {Nick, Packet}} ->
		       Robots = ?DICT:store(From, passed, StateData#state.robots),
		       add_new_user(From, Nick, Packet, StateData#state{robots=Robots});
		   _ ->
		       StateData
	       end,
    {next_state, normal_state, NewState};
handle_info({captcha_failed, From}, normal_state, StateData) ->
    NewState = case ?DICT:find(From, StateData#state.robots) of
		   {ok, {Nick, Packet}} ->
		       Robots = ?DICT:erase(From, StateData#state.robots),
		       Err = jlib:make_error_reply(
			       Packet, ?ERR_NOT_AUTHORIZED),
		       ejabberd_router:route( % TODO: s/Nick/""/
			 jlib:jid_replace_resource(
			   StateData#state.jid, Nick),
			 From, Err),
		       StateData#state{robots=Robots};
		   _ ->
		       StateData
	       end,
    {next_state, normal_state, NewState};
handle_info(Info, StateName, StateData) ->
    {_, _, NewState} = handler_call(handle_info, [Info], StateData),
    {next_state, StateName, NewState}.

%%----------------------------------------------------------------------
%% Func: terminate/3
%% Purpose: Shutdown the fsm
%% Returns: any
%%----------------------------------------------------------------------
terminate(_Reason, _StateName, StateData) ->
    ?DICT:fold(
       fun(J, _, _) ->
	       tab_remove_online_user(J, StateData)
       end, [], StateData#state.users),
    mod_muc:room_destroyed(StateData#state.host, StateData#state.room, self(),
			   StateData#state.server_host),
    ok.

%%%----------------------------------------------------------------------
%%% Internal functions
%%%----------------------------------------------------------------------

route(Pid, From, ToNick, Packet) ->
    gen_fsm:send_event(Pid, {route, From, ToNick, Packet}).

process_groupchat_message(From, {xmlelement, "message", Attrs, _Els} = Packet,
			  StateData) ->
    Lang = xml:get_attr_s("xml:lang", Attrs),
    case is_user_online(From, StateData) orelse
	is_user_allowed_message_nonparticipant(From, StateData) of
	true ->
	    {FromNick, Role} = get_participant_data(From, StateData),
	    case handler_call(process_groupchat_message, [From, Packet, FromNick, Role], StateData) of
	        {allow, Packet2, StateData1} ->
	            lists:foreach(
			      fun({_LJID, Info}) ->
				      ejabberd_router:route(
					jlib:jid_replace_resource(
					  StateData1#state.jid,
					  FromNick),
					Info#user.jid,
					Packet2)
			      end,
			      ?DICT:to_list(StateData1#state.users)),
			    NewStateData3 =
				add_message_to_history(FromNick,
						       Packet2,
						       StateData1),
			    {next_state, normal_state, NewStateData3};
			{drop, _, StateData1}  ->
			    {next_state, normal_state, StateData1};
			{deny, Reason, StateData1}  ->
			    E = ?ERRT_FORBIDDEN(
					   Lang,Reason),
			    ejabberd_router:route(
			      StateData#state.jid,
			      From,
			      jlib:make_error_reply(Packet, E)),
			    {next_state, normal_state, StateData1};
			{filter, {Packet2, Fun}, StateData1}->
			    lists:foreach(
			      fun({_LJID, Info}) ->
			          %{ok, FromInfo} = ?DICT:find(jlib:jid_tolower(From), StateData1#state.users),
			          case Fun(FromNick, Role , Info) of
			              true ->
				            ejabberd_router:route(
					        jlib:jid_replace_resource(
					          StateData#state.jid,
					          FromNick),
					        Info#user.jid,
					        Packet2);
					      _->
					        ok
					 end
				end,?DICT:to_list(StateData1#state.users)),
			    StateData2 = add_message_to_history(FromNick,
				    Packet2,
				    StateData1),
			    {next_state, normal_state, StateData2};
			 {error, Reason, StateData2}->
			     Err = jlib:make_error_reply(
		         Packet, ?ERRT_NOT_ACCEPTABLE(Lang, Reason)),
	             ejabberd_router:route(StateData#state.jid, From, Err),
	             {next_state, normal_state, StateData2}
			end;
	false ->
	    ErrText = "Only occupants are allowed to send messages to the conference",
	    Err = jlib:make_error_reply(
		    Packet, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)),
	    ejabberd_router:route(StateData#state.jid, From, Err),
	    {next_state, normal_state, StateData}
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Admin stuff
process_iq(UserInfo, Affiliation, ?NS_MUC_ADMIN, set, Lang, SubEl, State) ->
    {xmlelement, _, _, Items} = SubEl,
    process_admin_items_set(UserInfo,Affiliation, Items, Lang, State);


process_iq(#user{role = FRole} = UserInfo,FAffiliation, ?NS_MUC_ADMIN, get, Lang, SubEl,State) ->
    case xml:get_subtag(SubEl, "item") of
	false ->
	    {error, ?ERR_BAD_REQUEST};
	Item ->
	    case xml:get_tag_attr("role", Item) of
		false ->
		    case xml:get_tag_attr("affiliation", Item) of
			false ->
			    {error, ?ERR_BAD_REQUEST, State};
			{value, StrAffiliation} ->
			    case list_to_affiliation(StrAffiliation,State) of
				error ->
				    {error, ?ERR_BAD_REQUEST};
				SAffiliation ->
				    case handler_call(can_get_affiliations,[UserInfo, FAffiliation], State) of
				    {_, true, _} ->
					    Items = items_with_affiliation(
						      SAffiliation, State),
					    {result, Items, State};
					_ ->
					    ErrText = "Administrator privileges required : " ++ atom_to_list(FAffiliation),
					    {error, ?ERRT_FORBIDDEN(Lang, ErrText)}
				    end
			    end
		    end;
		{value, StrRole} ->
		    case list_to_role(StrRole, State) of
			error ->
			    {error, ?ERR_BAD_REQUEST};
			SRole ->
			    if
				FRole == moderator ->
				    Items = items_with_role(SRole, State),
				    {result, Items, State};
				true ->
				    ErrText = "Moderator privileges required",
				    {error, ?ERRT_FORBIDDEN(Lang, ErrText)}
			    end
		    end
	    end
    end;
    
process_iq(UserInfo,Aff, ?NS_MUC_OWNER , set, Lang, SubEl, State) ->
	    {xmlelement, _Name, _Attrs, Els} = SubEl,
	    case xml:remove_cdata(Els) of
		[{xmlelement, "x", _Attrs1, _Els1} = XEl] ->
		    case {xml:get_tag_attr_s("xmlns", XEl),
			  xml:get_tag_attr_s("type", XEl)} of
			{?NS_XDATA, "cancel"} ->
			    {result, [], State};
			{?NS_XDATA, "submit"} ->
				 case handler_call(set_config, [UserInfo, Aff, XEl, Lang], State) of
				      {result, _, NS} = R ->
				         mod_muc_room:add_to_log(roomconfig_change, [], NS),
				         R;
				      E -> E
				 end;
			_ ->
			    {error, ?ERR_BAD_REQUEST}
		    end;
		%%% TODO
		[{xmlelement, "destroy", _Attrs1, _Els1} = SubEl1] ->
		    ?INFO_MSG("Destroyed MUC room ~s by the owner ~s", 
			      [jlib:jid_to_string(State#state.jid), jlib:jid_to_string(UserInfo#user.jid)]),
		    mod_muc_room:destroy_room(SubEl1, State);
        _ ->
            {error, ?ERR_BAD_REQUEST}
	    end;
	
process_iq(UserInfo,Affiliation, ?NS_MUC_OWNER, get, Lang, _SubEl,State) ->
	handler_call(get_config,[UserInfo, Affiliation, Lang], State);
	
	
process_iq(UserInfo, FAffiliation, XMLNS, Type, Lang, SubEl, State)->
    handler_call(process_iq, [UserInfo, FAffiliation, XMLNS, Type, Lang, SubEl], State).

%% @doc Check if this non participant can send message to room.
%%
%% XEP-0045 v1.23:
%% 7.9 Sending a Message to All Occupants
%% an implementation MAY allow users with certain privileges
%% (e.g., a room owner, room admin, or service-level admin)
%% to send messages to the room even if those users are not occupants.
%%
%% Check the mod_muc option access_message_nonparticipant and wether this JID
%% is allowed or denied
is_user_allowed_message_nonparticipant(JID, StateData) ->
    case get_service_affiliation(JID, StateData) of
	owner ->
	    true;
	_ -> false
    end.

%% @doc Get information of this participant, or default values.
%% If the JID is not a participant, return values for a service message.
get_participant_data(From, StateData) ->
    case ?DICT:find(jlib:jid_tolower(From), StateData#state.users) of
	{ok, #user{nick = FromNick, role = Role}} ->
	    {FromNick, Role};
	error ->
	    {"", moderator}
    end.

get_user_info(From, StateData) ->
    case ?DICT:find(jlib:jid_tolower(From), StateData#state.users) of
    {ok, User} -> User;
    error -> {not_in_room, From}
    end.

process_presence(From, Nick, {xmlelement, "presence", Attrs, _Els} = Packet,
		StateData) ->
    Type = xml:get_attr_s("type", Attrs),
    Lang = xml:get_attr_s("xml:lang", Attrs),
    StateData1 =
	case Type of
	    "unavailable" ->
		case is_user_online(From, StateData) of
		    true ->
			NewState =
			    add_user_presence_un(From, Packet, StateData),
			send_new_presence(From, NewState),
			Reason = case xml:get_subtag(Packet, "status") of
				false -> "";
				Status_el -> xml:get_tag_cdata(Status_el)
			end,
			remove_online_user(From, NewState, Reason);
		    _ ->
			StateData
		end;
	    "error" ->
		case is_user_online(From, StateData) of
		    true ->
			ErrorText = "This participant is kicked from the room because "
			    "he sent an error presence",
			expulse_participant(Packet, From, StateData,
					    translate:translate(Lang, ErrorText));
		    _ ->
			StateData
		end;
	    "" ->
		case is_user_online(From, StateData) of
		    true ->
			case is_nick_change(From, Nick, StateData) of
			    true ->
				case {is_nick_exists(Nick, StateData),
				      mod_muc:can_use_nick(
					StateData#state.host, From, Nick),
					   handler_call(allow_nick_change, [From], StateData)} of
                {_, _, {false, ErrText,NewSD }} ->
					Err = jlib:make_error_reply(
						Packet,
						?ERRT_NOT_ALLOWED(Lang, ErrText)),
					ejabberd_router:route(
					  % TODO: s/Nick/""/
					  jlib:jid_replace_resource(
					    StateData#state.jid,
					    Nick),
					  From, Err),
					NewSD;
				    {true, _, _} ->
					Lang = xml:get_attr_s("xml:lang", Attrs),
					ErrText = "Nickname is already in use by another occupant",
					Err = jlib:make_error_reply(
						Packet,
						?ERRT_CONFLICT(Lang, ErrText)),
					ejabberd_router:route(
					  jlib:jid_replace_resource(
					    StateData#state.jid,
					    Nick), % TODO: s/Nick/""/
					  From, Err),
					StateData;
				    {_, false, _} ->
					ErrText = "Nickname is registered by another person",
					Err = jlib:make_error_reply(
						Packet,
						?ERRT_CONFLICT(Lang, ErrText)),
					ejabberd_router:route(
					  % TODO: s/Nick/""/
					  jlib:jid_replace_resource(
					    StateData#state.jid,
					    Nick),
					  From, Err),
					StateData;
				{_, _, {_, _, NewSD}} ->
					change_nick(From, Nick, NewSD)
				end;
			    _NotNickChange ->
			        case handler_call(process_presence, [From, Packet, Nick,Lang], StateData) of
			            {allow, Packet, NSD} ->
			                NewState = add_user_presence(From, Packet, NSD),
                            send_new_presence(From, NewState),
                            NewState;
                        {deny, Error, NSD} ->
                            Err = jlib:make_error_reply(Packet, Error),
                            ejabberd_router:route(
                                jlib:jid_replace_resource(
					                StateData#state.jid,
					                Nick),
					            From, Err),
                            NSD;
                        {drop, NSD} ->
                            NSD
                    end
                    
			end;
		    _ ->
			add_new_user(From, Nick, Packet, StateData)
		end;
	    _ ->
		StateData
	end,
    case (?DICT:to_list(StateData1#state.users) == []) of
	true ->
	    ?INFO_MSG("Destroyed MUC room ~s because it's empty", 
		      [jlib:jid_to_string(StateData#state.jid)]),
	    {stop, normal, StateData1};
	_ ->
	    {next_state, normal_state, StateData1}
    end.

is_user_online(JID, StateData) ->
    LJID = jlib:jid_tolower(JID),
    ?DICT:is_key(LJID, StateData#state.users).


%% Decide the fate of the message and its sender
%% Returns: continue_delivery | forget_message | {expulse_sender, Reason}
decide_fate_message("error", Packet, From, StateData) ->
    %% Make a preliminary decision
    PD = case check_error_kick(Packet) of
	     %% If this is an error stanza and its condition matches a criteria
	     true ->
		 Reason = io_lib:format("This participant is considered a ghost and is expulsed: ~s",
					[jlib:jid_to_string(From)]),
		 {expulse_sender, Reason};
	     false ->
		 continue_delivery
	 end,
    case PD of
	{expulse_sender, R} ->
	    case is_user_online(From, StateData) of
		true ->
		    {expulse_sender, R};
		false ->
		    forget_message
	    end;
	Other ->
	    Other
    end;

decide_fate_message(_, _, _, _) ->
    continue_delivery.

%% Check if the elements of this error stanza indicate
%% that the sender is a dead participant.
%% If so, return true to kick the participant.
check_error_kick(Packet) ->
    case get_error_condition(Packet) of
	"gone" -> true;
	"internal-server-error" -> true;
	"item-not-found" -> true;
	"jid-malformed" -> true;
	"recipient-unavailable" -> true;
	"redirect" -> true;
	"remote-server-not-found" -> true;
	"remote-server-timeout" -> true;
	"service-unavailable" -> true;
	_ -> false
    end.

get_error_condition(Packet) ->
	case catch get_error_condition2(Packet) of
	     {condition, ErrorCondition} ->
		ErrorCondition;
	     {'EXIT', _} ->
		"badformed error stanza"
	end.
get_error_condition2(Packet) ->
	{xmlelement, _, _, EEls} = xml:get_subtag(Packet, "error"),
	[Condition] = [Name || {xmlelement, Name, [{"xmlns", ?NS_STANZAS}], []} <- EEls],
	{condition, Condition}.

expulse_participant(Packet, From, StateData, Reason1) ->
	ErrorCondition = get_error_condition(Packet),
	Reason2 = io_lib:format(Reason1 ++ ": " ++ "~s", [ErrorCondition]),
	NewState = add_user_presence_un(
		From,
		{xmlelement, "presence",
		[{"type", "unavailable"}],
		[{xmlelement, "status", [],
		[{xmlcdata, Reason2}]
		}]},
	StateData),
	send_new_presence(From, NewState),
	remove_online_user(From, NewState).

remove_nonmembers(Headers) ->
    lists:foldl(
      fun({_LJID, #user{jid = JID}}, SD) ->
	    Affiliation = mod_muc_room:get_affiliation(JID, SD),
	    case Affiliation of
		none ->
		    catch mod_muc_room:send_kickban_presence(
			    JID, "", "322", SD),
		    mod_muc_room:set_role(JID, none, SD);
		_ ->
		    SD
	    end
      end, Headers, ?DICT:to_list(Headers#headers.users)).

process_admin_items_set(#user{jid = UJID, role=URole}, UAffiliation, Items, Lang,  State) ->
    case find_changed_items(UJID, UAffiliation, URole, Items, Lang, State, []) of
	{result, Res} ->
	    ?INFO_MSG("Processing MUC admin query from ~s in room ~s:~n ~p",
		      [jlib:jid_to_string(UJID), jlib:jid_to_string(State#state.jid), Res]),
	    NewState =
		lists:foldl(
		  fun({TJID, Type, Value, Reason}, SD) ->
			  case handler_call(process_changed_ra, [TJID, Type, Value, Reason], SD) of
			      {error, ErrReason, NSD} ->
				  ?ERROR_MSG("MUC ITEMS SET ERR: ~p~n",
					     [ErrReason]),
				  NSD;
			      NSD ->
				  NSD
			  end
		  end, State, Res),
	    {result, [], NewState};
	Err ->
	    Err
    end.


find_changed_items(_UJID, _UAffiliation, _URole, [], _Lang, _State, Res) ->
    {result, Res};
find_changed_items(UJID, UAffiliation, URole, [{xmlcdata, _} | Items],
		   Lang, State, Res) ->
    find_changed_items(UJID, UAffiliation, URole, Items, Lang, State, Res);
find_changed_items(UJID, UAffiliation, URole,
		   [{xmlelement, "item", Attrs, _Els} = Item | Items],
		   Lang, State, Res) ->
    TJID = case xml:get_attr("jid", Attrs) of
	       {value, S} ->
		   case jlib:string_to_jid(S) of
		       error ->
			   ErrText = io_lib:format(
				       translate:translate(
					 Lang,
					 "Jabber ID ~s is invalid"), [S]),
			   {error, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)};
		       J ->
			   {value, J}
		   end;
	       _ ->
		   case xml:get_attr("nick", Attrs) of
		       {value, N} ->
			   case find_jid_by_nick(N, State) of
			       false ->
				   ErrText =
				       io_lib:format(
					 translate:translate(
					   Lang,
					   "Nickname ~s does not exist in the room"),
					 [N]),
				   {error, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)};
			       J ->
				   {value, J}
			   end;
		       _ ->
			   {error, ?ERR_BAD_REQUEST}
		   end
	   end,
    case TJID of
	{value, JID} ->
	    TAffiliation = get_affiliation(JID, State),
	    TRole = get_role(JID, State),
	    case xml:get_attr("role", Attrs) of
		false ->
		    case xml:get_attr("affiliation", Attrs) of
			false ->
			    {error, ?ERR_BAD_REQUEST};
			{value, StrAffiliation} ->
			    case list_to_affiliation(StrAffiliation, State) of
				error ->
				    ErrText1 =
					io_lib:format(
					  translate:translate(
					    Lang,
					    "Invalid affiliation: ~s"),
					    [StrAffiliation]),
				    {error, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText1)};
				SAffiliation->
				    ServiceAf = mod_muc_room:get_service_affiliation(JID, State),
				    CanChangeRA =
					case handler_call(can_change_ra,[
					       UAffiliation, URole,
					       TAffiliation, TRole,
					       affiliation, SAffiliation,
						   ServiceAf], State) of
					    {result,nothing, _} ->
						    nothing;
					    {result,true, _} ->
						    true;
					    {result,check_owner, _} ->
						case search_affiliation(
						       owner, State) of
						    [{OJID, _}] ->
							jlib:jid_remove_resource(OJID) /=
							    jlib:jid_tolower(jlib:jid_remove_resource(UJID));
						    _ ->
							true
						end;
					    _ ->
						false
					end,
				    case CanChangeRA of
					nothing ->
					    find_changed_items(
					      UJID,
					      UAffiliation, URole,
					      Items, Lang, State,
					      Res);
					true ->
					    find_changed_items(
					      UJID,
					      UAffiliation, URole,
					      Items, Lang, State,
					      [{jlib:jid_remove_resource(JID),
						affiliation,
						SAffiliation,
						xml:get_path_s(
						  Item, [{elem, "reason"},
							 cdata])} | Res]);
					false ->
					    {error, ?ERR_NOT_ALLOWED}
				    end
			    end
		    end;
		{value, StrRole} ->
		    case list_to_role(StrRole,State) of
			error ->
			    ErrText1 =
				io_lib:format(
				  translate:translate(
				    Lang,
				    "Invalid role: ~s"),
				  [StrRole]),
			    {error, ?ERRT_BAD_REQUEST(Lang, ErrText1)};
			SRole->
			    ServiceAf = get_service_affiliation(JID, State),
			    CanChangeRA =
				case handler_call(can_change_ra,[
				       UAffiliation, URole,
				       TAffiliation, TRole,
				       role, SRole,
					   ServiceAf], State) of
				    {result, nothing, _ } ->
					nothing;
				    {result,true,_ }->
					true;
				    {result,check_owner,_ }->
					case search_affiliation(
					       owner, State) of
					    [{OJID, _}] ->
						jlib:jid_remove_resource(OJID) /=
						    jlib:jid_tolower(jlib:jid_remove_resource(UJID));
					    _ ->
						true
					end;
				    _ ->
					false
			    end,
			    case CanChangeRA of
				nothing ->
				    find_changed_items(
				      UJID,
				      UAffiliation, URole,
				      Items, Lang, State,
				      Res);
				true ->
				    find_changed_items(
				      UJID,
				      UAffiliation, URole,
				      Items, Lang, State,
				      [{JID, role, SRole,
					xml:get_path_s(
					  Item, [{elem, "reason"},
						 cdata])} | Res]);
				_ ->
				    {error, ?ERR_NOT_ALLOWED}
			    end
		    end
	    end;
	Err ->
	    Err
    end;
find_changed_items(_UJID, _UAffiliation, _URole, _Items,
		   _Lang, _State, _Res) ->
    {error, ?ERR_BAD_REQUEST}.

items_with_role(SRole, State) ->
    lists:map(
      fun({_, U}) ->
	      user_to_item(U, State)
      end, search_role(SRole, State)).

items_with_affiliation(SAffiliation, State) ->
    lists:map(
      fun({JID, {Affiliation, Reason}}) ->
	      {xmlelement, "item",
	       [{"affiliation", atom_to_list(Affiliation)},
		{"jid", jlib:jid_to_string(JID)}],
	       [{xmlelement, "reason", [], [{xmlcdata, Reason}]}]};
	 ({JID, Affiliation}) ->
	      {xmlelement, "item",
	       [{"affiliation", atom_to_list(Affiliation)},
		{"jid", jlib:jid_to_string(JID)}],
	       []}
      end, search_affiliation(SAffiliation, State)).

user_to_item(#user{role = Role,
		   nick = Nick,
		   jid = JID
		  },State) ->
    Affiliation = get_affiliation(JID, State),
    {xmlelement, "item",
     [{"role", atom_to_list(Role)},
      {"affiliation", atom_to_list(Affiliation)},
      {"nick", Nick},
      {"jid", jlib:jid_to_string(JID)}],
     []}.

search_role(Role, State) ->
    lists:filter(
      fun({_, #user{role = R}}) ->
	      Role == R
      end, ?DICT:to_list(State#state.users)).

search_affiliation(Affiliation, State) ->
    lists:filter(
      fun({_, A}) ->
	      case A of
		  {A1, _Reason} ->
		      Affiliation == A1;
		  _ ->
		      Affiliation == A
	      end
      end, ?DICT:to_list(State#state.affiliations)).


set_affiliation(JID,Affiliation,  #headers{} = Headers)->
    state_to_headers(set_affiliation(JID,Affiliation,headers_to_state(Headers)));
set_affiliation(JID, Affiliation, StateData) ->
    LJID = jlib:jid_remove_resource(jlib:jid_tolower(JID)),
    Affiliations = case Affiliation of
		       none ->
			   ?DICT:erase(LJID,
				       StateData#state.affiliations);
		       _ ->
			   ?DICT:store(LJID,
				       Affiliation,
				       StateData#state.affiliations)
		   end,
    StateData#state{affiliations = Affiliations}.

set_affiliation_and_reason(JID,Affiliation, Reason, #headers{} = Headers)->
    state_to_headers(set_affiliation_and_reason(JID,Affiliation, Reason,headers_to_state(Headers)));
set_affiliation_and_reason(JID, Affiliation, Reason, StateData) ->
    LJID = jlib:jid_remove_resource(jlib:jid_tolower(JID)),
    Affiliations = case Affiliation of
		       none ->
			   ?DICT:erase(LJID,
				       StateData#state.affiliations);
		       _ ->
			   ?DICT:store(LJID,
				       {Affiliation, Reason},
				       StateData#state.affiliations)
		   end,
    StateData#state{affiliations = Affiliations}.

get_affiliation(JID, #headers{} = Headers)->
    get_affiliation(JID,headers_to_state(Headers));
get_affiliation(JID, StateData) ->
    {_AccessRoute, _AccessCreate, AccessAdmin, _AccessPersistent} = StateData#state.access,
    Res =
	case acl:match_rule(StateData#state.server_host, AccessAdmin, JID) of
	    allow ->
		owner;
	    _ ->
		LJID = jlib:jid_tolower(JID),
		case ?DICT:find(LJID, StateData#state.affiliations) of
		    {ok, Affiliation} ->
			Affiliation;
		    _ ->
			LJID1 = jlib:jid_remove_resource(LJID),
			case ?DICT:find(LJID1, StateData#state.affiliations) of
			    {ok, Affiliation} ->
				Affiliation;
			    _ ->
				LJID2 = setelement(1, LJID, ""),
				case ?DICT:find(LJID2, StateData#state.affiliations) of
				    {ok, Affiliation} ->
					Affiliation;
				    _ ->
					LJID3 = jlib:jid_remove_resource(LJID2),
					case ?DICT:find(LJID3, StateData#state.affiliations) of
					    {ok, Affiliation} ->
						Affiliation;
					    _ ->
						none
					end
				end
			end
		end
	end,
    case Res of
	{A, _Reason} ->
	    A;
	_ ->
	    Res
    end.
    
get_service_affiliation(JID, #headers{} = Headers)->
    get_service_affiliation(JID,headers_to_state(Headers));
get_service_affiliation(JID, StateData) ->
    {_AccessRoute, _AccessCreate, AccessAdmin, _AccessPersistent} =
	StateData#state.access,
    case acl:match_rule(StateData#state.server_host, AccessAdmin, JID) of
	allow ->
	    owner;
	_ ->
	    none
    end.
    
set_role(JID, Role, #headers{} = Headers)->
    state_to_headers(set_role(JID,Role,headers_to_state(Headers)));
set_role(JID, Role, StateData) ->
    LJID = jlib:jid_tolower(JID),
    LJIDs = case LJID of
		{U, S, ""} ->
		    ?DICT:fold(
		       fun(J, _, Js) ->
			       case J of
				   {U, S, _} ->
				       [J | Js];
				   _ ->
				       Js
			       end
		       end, [], StateData#state.users);
		_ ->
		    case ?DICT:is_key(LJID, StateData#state.users) of
			true ->
			    [LJID];
			_ ->
			    []
		    end
	    end,
    Users = case Role of
		none ->
		    lists:foldl(fun(J, Us) ->
					?DICT:erase(J,
						    Us)
				end, StateData#state.users, LJIDs);
		_ ->
		    lists:foldl(fun(J, Us) ->
					{ok, User} = ?DICT:find(J, Us),
					?DICT:store(J,
						    User#user{role = Role},
						    Us)
				end, StateData#state.users, LJIDs)
	    end,
    StateData#state{users = Users}.

get_role(JID, #headers{} = Headers)->
    get_role(JID,headers_to_state(Headers));
get_role(JID, StateData) ->
    LJID = jlib:jid_tolower(JID),
    case ?DICT:find(LJID, StateData#state.users) of
	{ok, #user{role = Role}} ->
	    Role;
	_ ->
	    none
    end.

get_max_users_admin_threshold(StateData) ->
    gen_mod:get_module_opt(StateData#state.server_host,
			   mod_muc, max_users_admin_threshold, 5).

get_user_activity(JID, StateData) ->
    case treap:lookup(jlib:jid_tolower(JID),
		      StateData#state.activity) of
	{ok, _P, A} -> A;
	error ->
	    MessageShaper =
		shaper:new(gen_mod:get_module_opt(
			     StateData#state.server_host,
			     mod_muc, user_message_shaper, none)),
	    PresenceShaper =
		shaper:new(gen_mod:get_module_opt(
			     StateData#state.server_host,
			     mod_muc, user_presence_shaper, none)),
	    #activity{message_shaper = MessageShaper,
		      presence_shaper = PresenceShaper}
    end.

store_user_activity(JID, UserActivity, StateData) ->
    MinMessageInterval =
	gen_mod:get_module_opt(
	  StateData#state.server_host,
	  mod_muc, min_message_interval, 0),
    MinPresenceInterval =
	gen_mod:get_module_opt(
	  StateData#state.server_host,
	  mod_muc, min_presence_interval, 0),
    Key = jlib:jid_tolower(JID),
    Now = now_to_usec(now()),
    Activity1 = clean_treap(StateData#state.activity, {1, -Now}),
    Activity =
	case treap:lookup(Key, Activity1) of
	    {ok, _P, _A} ->
		treap:delete(Key, Activity1);
	    error ->
		Activity1
	end,
    StateData1 =
	case (MinMessageInterval == 0) andalso
	    (MinPresenceInterval == 0) andalso
	    (UserActivity#activity.message_shaper == none) andalso
	    (UserActivity#activity.presence_shaper == none) andalso
	    (UserActivity#activity.message == undefined) andalso
	    (UserActivity#activity.presence == undefined) of
	    true ->
		StateData#state{activity = Activity};
	    false ->
		case (UserActivity#activity.message == undefined) andalso
		    (UserActivity#activity.presence == undefined) of
		    true ->
			{_, MessageShaperInterval} =
			    shaper:update(UserActivity#activity.message_shaper,
					  100000),
			{_, PresenceShaperInterval} =
			    shaper:update(UserActivity#activity.presence_shaper,
					  100000),
			Delay = lists:max([MessageShaperInterval,
					   PresenceShaperInterval,
					   MinMessageInterval * 1000,
					   MinPresenceInterval * 1000]) * 1000,
			Priority = {1, -(Now + Delay)},
			StateData#state{
			  activity = treap:insert(
				       Key,
				       Priority,
				       UserActivity,
				       Activity)};
		    false ->
			Priority = {0, 0},
			StateData#state{
			  activity = treap:insert(
				       Key,
				       Priority,
				       UserActivity,
				       Activity)}
		end
	end,
    StateData1.

clean_treap(Treap, CleanPriority) ->
    case treap:is_empty(Treap) of
	true ->
	    Treap;
	false ->
	    {_Key, Priority, _Value} = treap:get_root(Treap),
	    if
		Priority > CleanPriority ->
		    clean_treap(treap:delete_root(Treap), CleanPriority);
		true ->
		    Treap
	    end
    end.


prepare_room_queue(StateData) ->
    case queue:out(StateData#state.room_queue) of
	{{value, {message, From}}, _RoomQueue} ->
	    Activity = get_user_activity(From, StateData),
	    Packet = Activity#activity.message,
	    Size = lists:flatlength(xml:element_to_string(Packet)),
	    {RoomShaper, RoomShaperInterval} =
		shaper:update(StateData#state.room_shaper, Size),
	    erlang:send_after(
	      RoomShaperInterval, self(),
	      process_room_queue),
	    StateData#state{
	      room_shaper = RoomShaper};
	{{value, {presence, From}}, _RoomQueue} ->
	    Activity = get_user_activity(From, StateData),
	    {_Nick, Packet} = Activity#activity.presence,
	    Size = lists:flatlength(xml:element_to_string(Packet)),
	    {RoomShaper, RoomShaperInterval} =
		shaper:update(StateData#state.room_shaper, Size),
	    erlang:send_after(
	      RoomShaperInterval, self(),
	      process_room_queue),
	    StateData#state{
	      room_shaper = RoomShaper};
	{empty, _} ->
	    StateData
    end.


add_online_user(JID, Nick, Role, StateData) ->
    LJID = jlib:jid_tolower(JID),
    Users = ?DICT:store(LJID,
			#user{jid = JID,
			      nick = Nick,
			      role = Role},
			StateData#state.users),
    add_to_log(join, Nick, StateData),
    tab_add_online_user(JID, StateData),
    StateData#state{users = Users}.

remove_online_user(JID, StateData) ->
	remove_online_user(JID, StateData, "").

remove_online_user(JID, StateData, Reason) ->
    LJID = jlib:jid_tolower(JID),
    {ok, #user{nick = Nick}} =
    	?DICT:find(LJID, StateData#state.users),
    add_to_log(leave, {Nick, Reason}, StateData),
    tab_remove_online_user(JID, StateData),
    Users = ?DICT:erase(LJID, StateData#state.users),
    StateData#state{users = Users}.


filter_presence({xmlelement, "presence", Attrs, Els}) ->
    FEls = lists:filter(
	     fun(El) ->
		     case El of
			 {xmlcdata, _} ->
			     false;
			 {xmlelement, _Name1, Attrs1, _Els1} ->
			     XMLNS = xml:get_attr_s("xmlns", Attrs1),
			     case XMLNS of
				 ?NS_MUC ++ _ ->
				     false;
				 _ ->
				     true
			     end
		     end
	     end, Els),
    {xmlelement, "presence", Attrs, FEls}.


add_user_presence(JID, Presence, StateData) ->
    LJID = jlib:jid_tolower(JID),
    FPresence = filter_presence(Presence),
    Users =
	?DICT:update(
	   LJID,
	   fun(#user{} = User) ->
		   User#user{last_presence = FPresence}
	   end, StateData#state.users),
    StateData#state{users = Users}.

add_user_presence_un(JID, Presence, StateData) ->
    LJID = jlib:jid_tolower(JID),
    FPresence = filter_presence(Presence),
    Users =
	?DICT:update(
	   LJID,
	   fun(#user{} = User) ->
		   User#user{last_presence = FPresence,
			     role = none}
	   end, StateData#state.users),
    StateData#state{users = Users}.


is_nick_exists(Nick, StateData) ->
    ?DICT:fold(fun(_, #user{nick = N}, B) ->
		       B orelse (N == Nick)
	       end, false, StateData#state.users).

find_jid_by_nick(Nick, #headers{} = Headers)->
    find_jid_by_nick(Nick, headers_to_state(Headers));
find_jid_by_nick(Nick, StateData) ->
    ?DICT:fold(fun(_, #user{jid = JID, nick = N}, R) ->
		       case Nick of
			   N -> JID;
			   _ -> R
		       end
	       end, false, StateData#state.users).

is_nick_change(JID, Nick, StateData) ->
    LJID = jlib:jid_tolower(JID),
    case Nick of
	"" ->
	    false;
	_ ->
	    {ok, #user{nick = OldNick}} =
		?DICT:find(LJID, StateData#state.users),
	    Nick /= OldNick
    end.

add_new_user(From, Nick, {xmlelement, _, Attrs, Els} = Packet, StateData) ->
    Lang = xml:get_attr_s("xml:lang", Attrs),
    MaxUsers= get_max_users(StateData),
    MaxAdminUsers = MaxUsers + get_max_users_admin_threshold(StateData),
    NUsers = dict:fold(fun(_, _, Acc) -> Acc + 1 end, 0,
		       StateData#state.users),
    ServiceAffiliation = get_service_affiliation(From, StateData),
    Affiliation =  get_affiliation(From, StateData),
    NConferences = tab_count_user(From),
    MaxConferences = gen_mod:get_module_opt(
		       StateData#state.server_host,
		       mod_muc, max_user_conferences, 10),
	CanJoin = handler_call(can_join,[From, Nick, Affiliation, ServiceAffiliation, Lang, Packet], StateData),
    case {( ServiceAffiliation == owner orelse
	   MaxUsers == none orelse
	   ((Affiliation == admin orelse Affiliation == owner) andalso
	    NUsers < MaxAdminUsers) orelse
	   NUsers < MaxUsers) andalso
	  NConferences < MaxConferences,
	  is_nick_exists(Nick, StateData),
	  mod_muc:can_use_nick(StateData#state.host, From, Nick),
	  CanJoin} of
	{false, _, _, _} ->
	    % max user reached and user is not admin or owner
	    Err = jlib:make_error_reply(
		    Packet,
		    ?ERR_SERVICE_UNAVAILABLE),
	    ejabberd_router:route( % TODO: s/Nick/""/
	      jlib:jid_replace_resource(StateData#state.jid, Nick),
	      From, Err),
	    StateData;
	{_, _, _, {false, Reason, _}} ->
	    Err = jlib:make_error_reply(
		    Packet, Reason),
	    ejabberd_router:route( % TODO: s/Nick/""/
	      jlib:jid_replace_resource(StateData#state.jid, Nick),
	      From, Err),
	    StateData;
	{_, true, _, _} ->
	    ErrText = "Nickname is already in use by another occupant",
	    Err = jlib:make_error_reply(Packet, ?ERRT_CONFLICT(Lang, ErrText)),
	    ejabberd_router:route(
	      % TODO: s/Nick/""/
	      jlib:jid_replace_resource(StateData#state.jid, Nick),
	      From, Err),
	    StateData;
	{_, _, false, _} ->
	    ErrText = "Nickname is registered by another person",
	    Err = jlib:make_error_reply(Packet, ?ERRT_CONFLICT(Lang, ErrText)),
	    ejabberd_router:route(
	      % TODO: s/Nick/""/
	      jlib:jid_replace_resource(StateData#state.jid, Nick),
	      From, Err),
	    StateData;
	{_, _, _, {true, Role, _}} ->
		 NewState =
		 add_user_presence(
		   From, Packet,
		   add_online_user(From, Nick, Role, StateData)),
		 case is_anonymous(NewState) of
		 false ->
		     WPacket = {xmlelement, "message", [{"type", "groupchat"}],
		 	       [{xmlelement, "body", [],
		 		 [{xmlcdata, translate:translate(
		 			       Lang,
		 			       "This room is not anonymous")}]},
		 		{xmlelement, "x", [{"xmlns", ?NS_MUC_USER}],
		 		 [{xmlelement, "status", [{"code", "100"}], []}]}]},
		     ejabberd_router:route(
		       StateData#state.jid,
		       From, WPacket);
		 true ->
		     ok
		 end,
		 send_existing_presences(From, NewState),
		 send_new_presence(From, NewState),
		 Shift = count_stanza_shift(Nick, Els, NewState),
		 case send_history(From, Shift, NewState) of
		 true ->
		     ok;
		 _ ->
		     send_subject(From, Lang, StateData)
		 end,
		 case NewState#state.just_created of
		 true ->
		     NewState#state{just_created = false};
		 false ->
		     Robots = ?DICT:erase(From, StateData#state.robots),
		     NewState#state{robots = Robots}
		 end
    end.

get_max_users(Headers) when is_record(Headers, headers) ->
    get_max_users(headers_to_state(Headers));
get_max_users(State)->
    ServiceMaxUsers = get_service_max_users(State), 
    MaxUsers = case handler_call(get_max_users, [], State) of
        {result, N, _} -> N;
        _-> ServiceMaxUsers
    end,
    if
	    MaxUsers =< ServiceMaxUsers -> MaxUsers;
	    true -> ServiceMaxUsers
    end.
    
    
get_service_max_users(ServerHost) ->
    gen_mod:get_module_opt(ServerHost,
			   mod_muc, max_users, ?MAX_USERS_DEFAULT).



count_stanza_shift(Nick, Els, StateData) ->
    HL = lqueue_to_list(StateData#state.history),
    Since = extract_history(Els, "since"),
    Shift0 = case Since of
		 false ->
		     0;
		 _ ->
		     Sin = calendar:datetime_to_gregorian_seconds(Since),
		     count_seconds_shift(Sin, HL)
	     end,
    Seconds = extract_history(Els, "seconds"),
    Shift1 = case Seconds of
		 false ->
		     0;
		 _ ->
		     Sec = calendar:datetime_to_gregorian_seconds(
			     calendar:now_to_universal_time(now())) - Seconds,
		     count_seconds_shift(Sec, HL)
	     end,
    MaxStanzas = extract_history(Els, "maxstanzas"),
    Shift2 = case MaxStanzas of
		 false ->
		     0;
		 _ ->
		     count_maxstanzas_shift(MaxStanzas, HL)
	     end,
    MaxChars = extract_history(Els, "maxchars"),
    Shift3 = case MaxChars of
		 false ->
		     0;
		 _ ->
		     count_maxchars_shift(Nick, MaxChars, HL)
	     end,
    lists:max([Shift0, Shift1, Shift2, Shift3]).

count_seconds_shift(Seconds, HistoryList) ->
    lists:sum(
      lists:map(
	fun({_Nick, _Packet, _HaveSubject, TimeStamp, _Size}) ->
	    T = calendar:datetime_to_gregorian_seconds(TimeStamp),
	    if
		T < Seconds ->
		    1;
		true ->
		    0
	    end
	end, HistoryList)).

count_maxstanzas_shift(MaxStanzas, HistoryList) ->
    S = length(HistoryList) - MaxStanzas,
    if
	S =< 0 ->
	    0;
	true ->
	    S
    end.

count_maxchars_shift(Nick, MaxSize, HistoryList) ->
    NLen = string:len(Nick) + 1,
    Sizes = lists:map(
	      fun({_Nick, _Packet, _HaveSubject, _TimeStamp, Size}) ->
		  Size + NLen
	      end, HistoryList),
    calc_shift(MaxSize, Sizes).

calc_shift(MaxSize, Sizes) ->
    Total = lists:sum(Sizes),
    calc_shift(MaxSize, Total, 0, Sizes).

calc_shift(_MaxSize, _Size, Shift, []) ->
    Shift;
calc_shift(MaxSize, Size, Shift, [S | TSizes]) ->
    if
	MaxSize >= Size ->
	    Shift;
	true ->
	    calc_shift(MaxSize, Size - S, Shift + 1, TSizes)
    end.

extract_history([], _Type) ->
    false;
extract_history([{xmlelement, _Name, Attrs, _SubEls} = El | Els], Type) ->
    case xml:get_attr_s("xmlns", Attrs) of
	?NS_MUC ->
	    AttrVal = xml:get_path_s(El,
		       [{elem, "history"}, {attr, Type}]),
	    case Type of
		"since" ->
		    case jlib:datetime_string_to_timestamp(AttrVal) of
			undefined ->
			    false;
			TS ->
			    calendar:now_to_universal_time(TS)
		    end;
		_ ->
		    case catch list_to_integer(AttrVal) of
			IntVal when is_integer(IntVal) and (IntVal >= 0) ->
			    IntVal;
			_ ->
			    false
		    end
	    end;
	_ ->
	    extract_history(Els, Type)
    end;
extract_history([_ | Els], Type) ->
    extract_history(Els, Type).


send_update_presence(JID, StateData) ->
    send_update_presence(JID, "", StateData).
send_update_presence(JID, Reason, Headers) when is_record(Headers, headers)->
    send_update_presence(JID, Reason, headers_to_state(Headers));
send_update_presence(JID, Reason, StateData) ->
    LJID = jlib:jid_tolower(JID),
    LJIDs = case LJID of
		{U, S, ""} ->
		    ?DICT:fold(
		       fun(J, _, Js) ->
			       case J of
				   {U, S, _} ->
				       [J | Js];
				   _ ->
				       Js
			       end
		       end, [], StateData#state.users);
		_ ->
		    case ?DICT:is_key(LJID, StateData#state.users) of
			true ->
			    [LJID];
			_ ->
			    []
		    end
	    end,
    lists:foreach(fun(J) ->
			  send_new_presence(J, Reason, StateData)
		  end, LJIDs).

send_new_presence(NJID, StateData) ->
    send_new_presence(NJID, "", StateData).
send_new_presence(JID, Reason, Headers) when is_record(Headers, headers)->
    send_new_presence(JID, Reason, headers_to_state(Headers));
send_new_presence(NJID, Reason, StateData) ->
    {ok, #user{jid = RealJID,
	       nick = Nick,
	       role = Role,
	       last_presence = Presence} = User } =
	?DICT:find(jlib:jid_tolower(NJID), StateData#state.users),
    Affiliation = get_affiliation(NJID, StateData),
    SAffiliation= atom_to_list(Affiliation),
    SRole= atom_to_list(Role),
    lists:foreach(
      fun({_LJID, Info}) ->
	      {result, CanGetJID,_} = handler_call(can_get_full_jids, [User], StateData),
		  ItemAttrs = case CanGetJID of
		      true ->
			  [{"jid", jlib:jid_to_string(RealJID)},
			   {"affiliation", SAffiliation},
			   {"role", SRole}];
		      _ ->
			  [{"affiliation", SAffiliation},
			   {"role", SRole}]
		  end,
	      ItemEls = case Reason of
			    "" ->
				[];
			    _ ->
				[{xmlelement, "reason", [],
				  [{xmlcdata, Reason}]}]
			end,
	      Status = case StateData#state.just_created of
			   true ->
			       [{xmlelement, "status", [{"code", "201"}], []}];
			   false ->
			       []
		       end,
	      Packet = append_subtags(
			 Presence,
			 [{xmlelement, "x", [{"xmlns", ?NS_MUC_USER}],
			   [{xmlelement, "item", ItemAttrs, ItemEls} | Status]}]),
	      ejabberd_router:route(
		jlib:jid_replace_resource(StateData#state.jid, Nick),
		Info#user.jid,
		Packet)
      end, ?DICT:to_list(StateData#state.users)).


send_existing_presences(ToJID,StateData) ->
    LToJID = jlib:jid_tolower(ToJID),
    {ok,  User = #user{jid = RealToJID} } =
	?DICT:find(LToJID, StateData#state.users),
    lists:foreach(
      fun({LJID, #user{jid = FromJID,
		       nick = FromNick,
		       role = FromRole,
		       last_presence = Presence
		      }}) ->
	      case RealToJID of
		  FromJID ->
		      ok;
		  _ ->
		      FromAffiliation = get_affiliation(LJID, StateData),
		      {result, CanGetJID,_} = handler_call(can_get_full_jids, [User], StateData),
		      ItemAttrs =
			  case CanGetJID of
			      true ->
				  [{"jid", jlib:jid_to_string(FromJID)},
				   {"affiliation",atom_to_list(FromAffiliation) },
				   {"role", atom_to_list(FromRole)}];
			      _ ->
				  [{"affiliation",atom_to_list(FromAffiliation) },
				   {"role", atom_to_list(FromRole)}]
			  end,
		      Packet = append_subtags(
				 Presence,
				 [{xmlelement, "x", [{"xmlns", ?NS_MUC_USER}],
				   [{xmlelement, "item", ItemAttrs, []}]}]),
		      ejabberd_router:route(
			jlib:jid_replace_resource(
			  StateData#state.jid, FromNick),
			RealToJID,
			Packet)
	      end
      end, ?DICT:to_list(StateData#state.users)).


append_subtags({xmlelement, Name, Attrs, SubTags1}, SubTags2) ->
    {xmlelement, Name, Attrs, SubTags1 ++ SubTags2}.


now_to_usec({MSec, Sec, USec}) ->
    (MSec*1000000 + Sec)*1000000 + USec.


change_nick(JID, Nick, StateData) ->
    LJID = jlib:jid_tolower(JID),
    {ok, #user{nick = OldNick}} =
	?DICT:find(LJID, StateData#state.users),
    Users =
	?DICT:update(
	   LJID,
	   fun(#user{} = User) ->
		   User#user{nick = Nick}
	   end, StateData#state.users),
    NewStateData = StateData#state{users = Users},
    send_nick_changing(JID, OldNick, NewStateData),
    add_to_log(nickchange, {OldNick, Nick}, StateData),
    NewStateData.

send_nick_changing(JID, OldNick, StateData) ->
    {ok, #user{jid = RealJID,
	       nick = Nick,
	       role = Role,
	       last_presence = Presence}} =
	?DICT:find(jlib:jid_tolower(JID), StateData#state.users),
    Affiliation = get_affiliation(JID, StateData),
    SAffiliation= atom_to_list(Affiliation),
    SRole = atom_to_list(Role),
    lists:foreach(
      fun({_LJID, Info}) ->
          {result, ShowFullJIDs, _} = handler_call(can_get_full_jids,[Info],StateData),
	      ItemAttrs1 =
		  case ShowFullJIDs of
		      true ->
			  [{"jid", jlib:jid_to_string(RealJID)},
			   {"affiliation", SAffiliation},
			   {"role", SRole},
			   {"nick", Nick}];
		      _ ->
			  [{"affiliation", SAffiliation},
			   {"role", SRole},
			   {"nick", Nick}]
		  end,
	      ItemAttrs2 =
		  case ShowFullJIDs of
		      true ->
			  [{"jid", jlib:jid_to_string(RealJID)},
			   {"affiliation", SAffiliation},
			   {"role", SRole}];
		      _ ->
			  [{"affiliation", SAffiliation},
			   {"role", SRole}]
		  end,
	      Packet1 =
		  {xmlelement, "presence", [{"type", "unavailable"}],
		   [{xmlelement, "x", [{"xmlns", ?NS_MUC_USER}],
		     [{xmlelement, "item", ItemAttrs1, []},
		      {xmlelement, "status", [{"code", "303"}], []}]}]},
	      Packet2 = append_subtags(
			  Presence,
			  [{xmlelement, "x", [{"xmlns", ?NS_MUC_USER}],
			    [{xmlelement, "item", ItemAttrs2, []}]}]),
	      ejabberd_router:route(
		jlib:jid_replace_resource(StateData#state.jid, OldNick),
		Info#user.jid,
		Packet1),
	      ejabberd_router:route(
		jlib:jid_replace_resource(StateData#state.jid, Nick),
		Info#user.jid,
		Packet2)
      end, ?DICT:to_list(StateData#state.users)).


lqueue_new(Max) ->
    #lqueue{queue = queue:new(),
	    len = 0,
	    max = Max}.

%% If the message queue limit is set to 0, do not store messages.
lqueue_in(_Item, LQ = #lqueue{max = 0}) ->
    LQ;
%% Otherwise, rotate messages in the queue store.
lqueue_in(Item, #lqueue{queue = Q1, len = Len, max = Max}) ->
    Q2 = queue:in(Item, Q1),
    if
	Len >= Max ->
	    Q3 = lqueue_cut(Q2, Len - Max + 1),
	    #lqueue{queue = Q3, len = Max, max = Max};
	true ->
	    #lqueue{queue = Q2, len = Len + 1, max = Max}
    end.

lqueue_cut(Q, 0) ->
    Q;
lqueue_cut(Q, N) ->
    {_, Q1} = queue:out(Q),
    lqueue_cut(Q1, N - 1).

lqueue_to_list(#lqueue{queue = Q1}) ->
    queue:to_list(Q1).


add_message_to_history(FromNick, Packet, StateData) ->
    HaveSubject = case xml:get_subtag(Packet, "subject") of
		      false ->
			  false;
		      _ ->
			  true
		  end,
    TimeStamp = calendar:now_to_universal_time(now()),
    TSPacket = append_subtags(Packet,
			      [jlib:timestamp_to_xml(TimeStamp)]),
    SPacket = jlib:replace_from_to(
		jlib:jid_replace_resource(StateData#state.jid, FromNick),
		StateData#state.jid,
		TSPacket),
    Size = lists:flatlength(xml:element_to_string(SPacket)),
    Q1 = lqueue_in({FromNick, TSPacket, HaveSubject, TimeStamp, Size},
		   StateData#state.history),
    add_to_log(text, {FromNick, Packet}, StateData),
    StateData#state{history = Q1}.

send_history(JID, Shift, StateData) ->
    lists:foldl(
      fun({Nick, Packet, HaveSubject, _TimeStamp, _Size}, B) ->
	      ejabberd_router:route(
		jlib:jid_replace_resource(StateData#state.jid, Nick),
		JID,
		Packet),
	      B or HaveSubject
      end, false, lists:nthtail(Shift, lqueue_to_list(StateData#state.history))).


send_subject(JID, Lang, StateData) ->
    case StateData#state.subject_author of
	"" ->
	    ok;
	Nick ->
	    Subject = StateData#state.subject,
	    Packet = {xmlelement, "message", [{"type", "groupchat"}],
		      [{xmlelement, "subject", [], [{xmlcdata, Subject}]},
		       {xmlelement, "body", [],
			[{xmlcdata,
			  Nick ++
			  translate:translate(Lang,
					      " has set the subject to: ") ++
			  Subject}]}]},
	    ejabberd_router:route(
	      StateData#state.jid,
	      JID,
	      Packet)
    end.

send_kickban_presence(JID, Reason, Code, Headers) ->
    LJID = jlib:jid_tolower(JID),
    LJIDs = case LJID of
		{U, S, ""} ->
		    ?DICT:fold(
		       fun(J, _, Js) ->
			       case J of
				   {U, S, _} ->
				       [J | Js];
				   _ ->
				       Js
			       end
		       end, [], Headers#headers.users);
		_ ->
		    case ?DICT:is_key(LJID, Headers#headers.users) of
			true ->
			    [LJID];
			_ ->
			    []
		    end
	    end,
    lists:foreach(fun(J) ->
			  {ok, #user{nick = Nick}} =
			      ?DICT:find(J, Headers#headers.users),
			  add_to_log(kickban, {Nick, Reason, Code}, headers_to_state(Headers)),
			  tab_remove_online_user(J, headers_to_state(Headers)),
			  send_kickban_presence1(J, Reason, Code, Headers)
		  end, LJIDs).

send_kickban_presence1(UJID, Reason, Code, Headers) ->
    {ok, #user{jid = _RealJID,
	       nick = Nick}} =
	?DICT:find(jlib:jid_tolower(UJID), Headers#headers.users),
    Affiliation = get_affiliation(UJID, Headers),
    %TODO
    SAffiliation = atom_to_list(Affiliation),
    lists:foreach(
      fun({_LJID, Info}) ->
	      ItemAttrs = [{"affiliation", SAffiliation},
			   {"role", "none"}],
	      ItemEls = case Reason of
			    "" ->
				[];
			    _ ->
				[{xmlelement, "reason", [],
				  [{xmlcdata, Reason}]}]
			end,
	      Packet = {xmlelement, "presence", [{"type", "unavailable"}],
			[{xmlelement, "x", [{"xmlns", ?NS_MUC_USER}],
			  [{xmlelement, "item", ItemAttrs, ItemEls},
			   {xmlelement, "status", [{"code", Code}], []}]}]},
	      ejabberd_router:route(
		jlib:jid_replace_resource(Headers#headers.jid, Nick),
		Info#user.jid,
		Packet)
      end, ?DICT:to_list(Headers#headers.users)).


destroy_room(DEl, #headers{}=Headers) ->
    destroy_room(DEl, headers_to_state(Headers));
destroy_room(DEl, StateData) ->
    lists:foreach(
      fun({_LJID, Info}) ->
	      Nick = Info#user.nick,
	      ItemAttrs = [{"affiliation", "none"},
			   {"role", "none"}],
	      Packet = {xmlelement, "presence", [{"type", "unavailable"}],
			[{xmlelement, "x", [{"xmlns", ?NS_MUC_USER}],
			  [{xmlelement, "item", ItemAttrs, []}, DEl]}]},
	      ejabberd_router:route(
		jlib:jid_replace_resource(StateData#state.jid, Nick),
		Info#user.jid,
		Packet)
      end, ?DICT:to_list(StateData#state.users)),
    case is_persistent(StateData) of
	true ->
	    mod_muc:forget_room(StateData#state.server_host, StateData#state.host, StateData#state.room);
	false ->
	    ok
	end,
    {result, [], stop}.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Plugin management
%% @doc <p>handler plugin call.</p>
handler_call(Function, Args, State)->
    handler_call(Function, Args, State, State#state.handler ).
handler_call(Function, Args, State, Handler) when is_record(State, state) ->
    %?DEBUG("Calling : ~p:~p(~p)", [Handler, Function, Args]),
    Headers = state_to_headers(State),
    case catch apply(Handler, Function, lists:append(Args, [Headers])) of
    {'EXIT', {undef, Undefined}} ->
	    case State#state.handler of
	        ?DEFAULT_HANDLER -> {error, {undef, Undefined}, State};
	        _ -> handler_call( Function, Args, State, ?DEFAULT_HANDLER)
	    end;
	{'EXIT', Reason} -> {error, Reason, State};
	{Return, Result} -> {Return, Result, State};
	{Return, Result, NewHeaders}-> 
	    if Function /= init -> %% do not store on init.
	        maybe_persist(State#state.handler, Headers, NewHeaders);
	      true -> ok
	    end,
	    {Return, Result, headers_to_state(NewHeaders, State)};
	NewHeaders when is_record(NewHeaders, headers) ->
	    headers_to_state(NewHeaders, State);
	Result -> {result, Result, State}
    end.
maybe_persist(_Handler, O, O) -> ok; %No change
maybe_persist(Handler, O, #headers{storage=S}=N) ->
    case {catch Handler:is_persistent(O), catch Handler:is_persistent(N), O /= N} of
	{_, true, true} ->
	    S:store_room(N#headers.server_host, N#headers.host, N#headers.room, Handler, Handler:make_opts(N));
	{true, false, _} ->
	    S:forget_room(N#headers.server_host, N#headers.host, N#headers.room);
	{_, _, _} ->
	    ok
    end.

list_to_affiliation(StrAff, State)->
    case handler_call(list_to_affiliation, [StrAff], State) of
        {result, Aff, _} -> Aff;
        _-> error
    end.
    
list_to_role(StrRole, State)->
    case handler_call(list_to_role, [StrRole], State) of
        {result, Role, _} -> Role;
        _-> error
    end.
is_persistent(State)->
    case handler_call(is_persistent, [], State) of
        {result, true, _} -> true;
        _-> false
    end.
is_anonymous(State)->
    case handler_call(is_anonymous, [], State) of
        {result, true, _} -> true;
        _-> false
    end.

%% fetching readonly values
headers_to_state(Headers)->
    headers_to_state(Headers, #state{
        host=Headers#headers.host,
        server_host = Headers#headers.server_host,
        access = Headers#headers.access,
        jid = Headers#headers.jid,
        room=Headers#headers.room,
        storage=Headers#headers.storage,
        handler=Headers#headers.handler
    }).
headers_to_state(Headers, State)  when is_record(State, state) ->
    State#state{
        subject=Headers#headers.subject,
        subject_author=Headers#headers.subject_author,
        robots=Headers#headers.robots,
        users=Headers#headers.users,
        affiliations=Headers#headers.affiliations,
        config=Headers#headers.config
    }.

state_to_headers(State) when is_record(State, state)->
    #headers{
        subject=State#state.subject,
        subject_author=State#state.subject_author,
        affiliations=State#state.affiliations,
        users=State#state.users,
        robots=State#state.robots,
        jid=State#state.jid,
        room=State#state.room,
        config=State#state.config,
        host=State#state.host,
        server_host=State#state.server_host,
        access=State#state.access,
        storage=State#state.storage,
        handler=State#state.handler
    }.
    
    
check_subject(Packet) ->
    case xml:get_subtag(Packet, "subject") of
	false ->
	    false;
	SubjEl ->
	    xml:get_tag_cdata(SubjEl)
    end.
    

extract_password([]) ->
    false;
extract_password([{xmlelement, _Name, Attrs, _SubEls} = El | Els]) ->
    case xml:get_attr_s("xmlns", Attrs) of
	?NS_MUC ->
	    case xml:get_subtag(El, "password") of
		false ->
		    false;
		SubEl ->
		    xml:get_tag_cdata(SubEl)
	    end;
	_ ->
	    extract_password(Els)
    end;
extract_password([_ | Els]) ->
    extract_password(Els).

strip_status({xmlelement, "presence", Attrs, Els}) ->
    FEls = lists:filter(
	     fun({xmlelement, "status", _Attrs1, _Els1}) ->
                     false;
                (_) -> true
	     end, Els),
    {xmlelement, "presence", Attrs, FEls}.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Invitation support 

check_invitation(From, Els, Lang, StateData) ->
    FAffiliation = get_affiliation(From, StateData),
    InviteEl = case xml:remove_cdata(Els) of
		   [{xmlelement, "x", _Attrs1, Els1} = XEl] ->
		       case xml:get_tag_attr_s("xmlns", XEl) of
			   ?NS_MUC_USER ->
			       ok;
			   _ ->
			       throw({error, ?ERR_BAD_REQUEST})
		       end,
		       case xml:remove_cdata(Els1) of
			   [{xmlelement, "invite", _Attrs2, _Els2} = InviteEl1] ->
			       InviteEl1;
			   _ ->
			       throw({error, ?ERR_BAD_REQUEST})
		       end;
		   _ ->
		       throw({error, ?ERR_BAD_REQUEST})
	       end,
    JID = case jlib:string_to_jid(
		 xml:get_tag_attr_s("to", InviteEl)) of
	      error ->
		  throw({error, ?ERR_JID_MALFORMED});
	      JID1 ->
		  JID1
	  end,
    case handler_call(can_invite, [From, FAffiliation,JID, InviteEl, Lang], StateData) of
	{false, _, _} ->
	    throw({error, ?ERR_NOT_ALLOWED});
	{true, Msg, NSD} ->
	    ejabberd_router:route(StateData#state.jid, JID, Msg),
	    NSD
    end.

%% Handle a message sent to the room by a non-participant.
%% If it is a decline, send to the inviter.
%% Otherwise, an error message is sent to the sender.
handle_roommessage_from_nonparticipant(Packet, Lang, StateData, From) ->
    case catch check_decline_invitation(Packet) of
	{true, Decline_data} ->
	    send_decline_invitation(Decline_data, StateData#state.jid, From);
	_ ->
	    send_error_only_occupants(Packet, Lang, StateData#state.jid, From)
    end.

%% Check in the packet is a decline.
%% If so, also returns the splitted packet.
%% This function must be catched, 
%% because it crashes when the packet is not a decline message.
check_decline_invitation(Packet) ->
    {xmlelement, "message", _, _} = Packet,
    XEl = xml:get_subtag(Packet, "x"),
    ?NS_MUC_USER = xml:get_tag_attr_s("xmlns", XEl),
    DEl = xml:get_subtag(XEl, "decline"),
    ToString = xml:get_tag_attr_s("to", DEl),
    ToJID = jlib:string_to_jid(ToString),
    {true, {Packet, XEl, DEl, ToJID}}.

%% Send the decline to the inviter user.
%% The original stanza must be slightly modified.
send_decline_invitation({Packet, XEl, DEl, ToJID}, RoomJID, FromJID) ->
    FromString = jlib:jid_to_string(FromJID),
    {xmlelement, "decline", DAttrs, DEls} = DEl,
    DAttrs2 = lists:keydelete("to", 1, DAttrs),
    DAttrs3 = [{"from", FromString} | DAttrs2],
    DEl2 = {xmlelement, "decline", DAttrs3, DEls},
    XEl2 = replace_subelement(XEl, DEl2),
    Packet2 = replace_subelement(Packet, XEl2),
    ejabberd_router:route(RoomJID, ToJID, Packet2).

%% Given an element and a new subelement, 
%% replace the instance of the subelement in element with the new subelement.
replace_subelement({xmlelement, Name, Attrs, SubEls}, NewSubEl) ->
    {_, NameNewSubEl, _, _} = NewSubEl,
    SubEls2 = lists:keyreplace(NameNewSubEl, 2, SubEls, NewSubEl),
    {xmlelement, Name, Attrs, SubEls2}.

send_error_only_occupants(Packet, Lang, RoomJID, From) ->
    ErrText = "Only occupants are allowed to send messages to the conference",
    Err = jlib:make_error_reply(Packet, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)),
    ejabberd_router:route(RoomJID, From, Err).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Logging
add_to_log(Type, Data, Headers) when is_record(Headers, headers) ->
    add_to_log(Type, Data, headers_to_state(Headers));
add_to_log(Type, Data, StateData) ->
    case handler_call(should_log, [], StateData) of
	{result, true, _} ->
	    mod_muc_log:add_to_log(
	      StateData#state.server_host, Type, Data,
	      StateData#state.jid, StateData);
	_ ->
	    ok
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Users number checking

tab_add_online_user(JID, StateData) ->
    {LUser, LServer, _} = jlib:jid_tolower(JID),
    US = {LUser, LServer},
    Room = StateData#state.room,
    Host = StateData#state.host,
    catch ets:insert(
	    muc_online_users,
	    #muc_online_users{us = US, room = Room, host = Host}).


tab_remove_online_user(JID, StateData) ->
    {LUser, LServer, _} = jlib:jid_tolower(JID),
    US = {LUser, LServer},
    Room = StateData#state.room,
    Host = StateData#state.host,
    catch ets:delete_object(
	    muc_online_users,
	    #muc_online_users{us = US, room = Room, host = Host}).

tab_count_user(JID) ->
    {LUser, LServer, _} = jlib:jid_tolower(JID),
    US = {LUser, LServer},
    case catch ets:select(
		 muc_online_users,
		 [{#muc_online_users{us = US, _ = '_'}, [], [[]]}]) of
	Res when is_list(Res) ->
	    length(Res);
	_ ->
	    0
    end.
