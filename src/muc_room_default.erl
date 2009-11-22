-module(muc_room_default).
-author('eric@ohmforce.com').

-behaviour(gen_muc_handler).

-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_muc_room.hrl"). 
-record(config, {title = "",
		 description = "",
		 allow_change_subj = true,
		 allow_query_users = true,
		 allow_private_messages = true,
		 allow_visitor_status = true,
		 allow_visitor_nickchange = true,
		 persistent = false,
		 max_users = ?MAX_USERS_DEFAULT,
		 public = true,
		 public_list = true,
		 moderated = true,
		 captcha_protected = false,
		 members_by_default = true,
		 members_only = false,
		 allow_user_invites = false,
		 password_protected = false,
		 password = "",
		 anonymous = true,
		 logging = false,
		 custom = []
		}).
-export([process_groupchat_message/5,
         process_private_message/4,
         process_iq/7,
         process_presence/5,
         process_user_iq/5,
         get_config/4,
         set_config/5,
         init/2,
         init/4]).
-export([
         process_changed_ra/5,
         can_join/7,
         can_change_ra/8,
         can_invite/6,
         is_password_protected/1,
         is_captcha_protected/1,
         is_persistent/1,
         is_anonymous/1,
         get_max_users/1,
         can_get_affiliations/3,
         allow_nick_change/2,
         should_log/1,
         can_get_full_jids/2,
         handle_info/2,
         handle_sync_event/3,
         make_opts/1,
         list_to_role/2,
         list_to_affiliation/2,
         get_disco_info/4,
         get_disco_item/4]).



can_invite(From, FAffiliation, To, InviteEl, Lang, Headers)->
    case ((Headers#headers.config)#config.allow_user_invites
	orelse (FAffiliation == admin) orelse (FAffiliation == owner)) of
	false-> {false, []};
	true -> 
	    Reason =
		xml:get_path_s(
		  InviteEl,
		  [{elem, "reason"}, cdata]),
	    ContinueEl =
		case xml:get_path_s(
		       InviteEl,
		       [{elem, "continue"}]) of
		    [] -> [];
		    Continue1 -> [Continue1]
		end,
	    IEl =
		[{xmlelement, "invite",
		  [{"from",
		    jlib:jid_to_string(From)}],
		  [{xmlelement, "reason", [],
		    [{xmlcdata, Reason}]}] ++ ContinueEl}],
	    PasswdEl =
		case (Headers#headers.config)#config.password_protected of
		    true ->
			[{xmlelement, "password", [],
			  [{xmlcdata, (Headers#headers.config)#config.password}]}];
		    _ ->
			[]
		end,
	    Body =
		{xmlelement, "body", [],
		 [{xmlcdata,
		   lists:flatten(
		     io_lib:format(
		       translate:translate(
			 Lang,
			 "~s invites you to the room ~s"),
		       [jlib:jid_to_string(From),
			jlib:jid_to_string({Headers#headers.room,
					    Headers#headers.host,
					    ""})
		       ])) ++
		   case (Headers#headers.config)#config.password_protected of
		       true ->
			   ", " ++
			       translate:translate(Lang, "the password is") ++
			       " '" ++
			       (Headers#headers.config)#config.password ++ "'";
		       _ ->
			   ""
		   end ++
		   case Reason of
		       "" -> "";
		       _ -> " (" ++ Reason ++ ") "
		   end
		  }]},
		  Config = Headers#headers.config,
		  NewHeaders = case Config#config.members_only of
		      true -> 
		      case mod_muc_room:get_affiliation(To, Headers) of
					none ->
					    mod_muc_room:set_affiliation(
						    To,
						    member,
						    Headers);
					_ -> Headers
			  end;
			  false -> Headers
		end,
		{true, {xmlelement, "message",
		 [{"type", "normal"}],
		 [{xmlelement, "x", [{"xmlns", ?NS_MUC_USER}], IEl ++ PasswdEl},
		  {xmlelement, "x",
		   [{"xmlns", ?NS_XCONFERENCE},
		    {"jid", jlib:jid_to_string({Headers#headers.room, Headers#headers.host,""})}],
		   [{xmlcdata, Reason}]},
		  Body]}, NewHeaders}
    end.
    
    
allow_nick_change(From, Headers)->
    case {(Headers#headers.config)#config.allow_visitor_nickchange,
            is_visitor(From, Headers)} of
        {false, true} ->
            ErrText = "Visitors are not allowed to change their nicknames in this room",
            {false, ErrText};
        _ ->
            true
    end.
should_log(Headers)->
    (Headers#headers.config)#config.logging.
is_persistent(#headers{config = Config})-> 
    Config#config.persistent.
get_max_users(#headers{config = Config})-> 
    Config#config.max_users.
is_anonymous(#headers{config = Config})-> 
    Config#config.anonymous.
is_captcha_protected(#headers{config = Config})-> 
    Config#config.captcha_protected.
is_password_protected(#headers{config = Config})->
    if Config#config.password_protected ->
        {true, Config#config.password};
       true ->
           false
    end.

can_get_affiliations(_UserInfo, admin, _Headers)-> true;
can_get_affiliations(_UserInfo, owner, _Headers)-> true;
can_get_affiliations(_, _FAff, _ ) -> false.
 
can_get_full_jids(Info, Headers)->
    (Info#user.role == moderator) orelse ((Headers#headers.config)#config.anonymous == false).
get_title(Headers) ->
    case (Headers#headers.config)#config.title of
	"" ->
	    Headers#headers.room;
	Name ->
	    Name
    end.

can_join(_JID,_Nick, _Aff,owner, _Lang, _Packet, Headers) ->
    {true, get_default_role(owner, Headers)};
    
can_join(_JID,_Nick, outcast,_ServiceAffiliation, Lang,_Packet, _Headers) ->
    ErrText = "You have been banned from this room",
    {false, ?ERRT_FORBIDDEN(Lang, ErrText)};
    
can_join(From,Nick, Affiliation, ServiceAffiliation, Lang, {xmlelement, _, Attrs, Els}=Packet,Headers)->
    case get_default_role(Affiliation, Headers) of
        none ->
            ErrText = "Membership required to enter this room",
			{false,?ERRT_REGISTRATION_REQUIRED(Lang, ErrText)};
		Role ->
		    case check_password(ServiceAffiliation, Affiliation,
				Els, From, Headers) of
		    true ->
		        {true, Role};
		    nopass ->
		        ErrText = "Password required to enter this room",
		        {false, ?ERRT_NOT_AUTHORIZED(Lang, ErrText)};
		    captcha_required ->
		        ID = randoms:get_string(),
		        SID = xml:get_attr_s("id", Attrs),
		        RoomJID = Headers#headers.jid,
		        To = jlib:jid_replace_resource(RoomJID, Nick),
		        case ejabberd_captcha:create_captcha(
			       ID, SID, RoomJID, To, Lang, From) of
			    {ok, CaptchaEls} ->
			        MsgPkt = {xmlelement, "message", [{"id", ID}], CaptchaEls},
			        Robots = ?DICT:store(From,
			    			 {Nick, Packet}, Headers#headers.robots),
			        ejabberd_router:route(RoomJID, From, MsgPkt),
			        Headers#headers{robots = Robots};
			    error ->
			        ErrText = "Unable to generate a captcha",
			        Err = jlib:make_error_reply(
			    	    Packet, ?ERRT_INTERNAL_SERVER_ERROR(Lang, ErrText)),
			        ejabberd_router:route( % TODO: s/Nick/""/
			          jlib:jid_replace_resource(
			    	Headers#headers.jid, Nick),
			          From, Err),
			        Headers
		        end;
		    _->
		      ErrText = "Incorrect password",
		        {false, ?ERRT_NOT_AUTHORIZED(Lang, ErrText)}
		    end
	end.
	
%%%% 
%%%% Subset of gen_fsm callbacks
%%%%
init(Opts, Creator, _Nick, Headers)-> 
    NH = mod_muc_room:set_affiliation(Creator, owner, Headers),
    NewHeaders= NH#headers{config = #config{}},
    {result, ok, set_opts(Opts, NewHeaders)}.
init(Opts, Headers)-> 
    NewHeaders= Headers#headers{config = #config{}},
    {result, ok, set_opts(Opts, NewHeaders)}.
    
handle_info(_Info, _Headers)->
    ok.

handle_sync_event(Event, From, Headers)->
    {error, not_implemented, Headers}.
    
%%%%%
%%%%% Stanza and event processing
%%%%%
process_private_message(_From, _ToNick, Packet, Headers)->
    if (Headers#headers.config)#config.allow_private_messages == false ->
        Error = "It is not allowed to send private messages",
        {deny, Error};
    true ->
       {allow, Packet}
    end. 

process_groupchat_message(_From, Packet, Nick, Role, Headers)->
        if
		(Role == moderator) or (Role == participant) 
		or ((Headers#headers.config)#config.moderated == false) ->
		    {Headers1, IsAllowed} =
			case mod_muc_room:check_subject(Packet) of
			    false ->
				{Headers, true};
			    Subject ->
				case can_change_subject(Role,
							Headers) of
				    true ->
					NewHeaders =
					    Headers#headers{
					      subject = Subject,
					      subject_author =  Nick},
					{NewHeaders, true};
				    _ ->
					{Headers, false}
				end
			end,
		    case IsAllowed of
			true ->
			    {allow, Packet, Headers1};
			_ ->
			    Err =
				case (Headers1#headers.config)#config.allow_change_subj of
				    true ->
					   "Only moderators and participants are allowed to change subject in this room";
				    _ ->
					   "Only moderators are allowed to change subject in this room"
				end,
				{deny, Err, Headers1}
		    end;
		true ->
		    ErrText = "Visitors are not allowed to send messages to all occupants",
		    {deny, ErrText, Headers}
	    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Query to user not room
process_user_iq(_From, _ToNick,Lang, Packet, Headers)->
    if (Headers#headers.config)#config.allow_query_users ->
        {allow, Packet};
    true ->
        ErrText = "Only occupants are allowed to send queries to the conference",
        {deny, ?ERRT_NOT_ACCEPTABLE(Lang, ErrText)}
    end.
    

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Owner stuff
check_allowed_log_change(XEl, Headers, From) ->
    case lists:keymember("muc#roomconfig_enablelogging", 1,
			 jlib:parse_xdata_submit(XEl)) of
	false ->
	    allow;
	true ->
	    mod_muc_log:check_access_log(
	      Headers#headers.server_host, From)
    end.

check_allowed_persistent_change(XEl, Headers, From) ->
    case lists:keymember("muc#roomconfig_persistentroom", 1,
			 jlib:parse_xdata_submit(XEl)) of
	false ->
	    allow;
	true ->
		{_AccessRoute, _AccessCreate, _AccessAdmin, AccessPersistent} = Headers#headers.access,
		acl:match_rule(Headers#headers.server_host, AccessPersistent, From)
    end.


    
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Disco

process_iq(_UserInfo, _Aff, ?NS_DISCO_INFO, set, _Lang, _SubEl,Headers) ->
    {error, ?ERR_NOT_ALLOWED, Headers};
    
process_iq({not_in_room, From}, _FAffiliation, ?NS_DISCO_INFO=XMLNS, get=Type, Lang, SubEl, Headers)->
    process_iq(#user{role=none, jid=From}, none, XMLNS, Type, Lang, SubEl, Headers);
process_iq(UserInfo, _Aff, ?NS_DISCO_INFO, get, Lang, _SubEl, Headers) ->
    Config = Headers#headers.config,
    {result, get_disco_info(UserInfo, Lang, Config, Headers), Headers};

process_iq(_UserInfo, _Aff, ?NS_DISCO_ITEMS, set, _Lang, _SubEl, _Headers) ->
    {error, ?ERR_NOT_ALLOWED};
    
process_iq({not_in_room, _From}, _FAffiliation, ?NS_DISCO_ITEMS=XMLNS, get=Type, Lang, SubEl, Headers)->
    process_iq(#user{role=none}, none, XMLNS, Type, Lang, SubEl, Headers);
    
process_iq(#user{role=FRole}, FAffiliation, ?NS_DISCO_ITEMS, get, _Lang, _SubEl, Headers) ->
    case ((Headers#headers.config)#config.public_list == true) orelse
	(FRole /= none) orelse
	(FAffiliation == admin) orelse
	(FAffiliation == owner) of
	true ->
	    UList =
		lists:map(
		  fun({_LJID, Info}) ->
			  Nick = Info#user.nick,
			  {xmlelement, "item",
			   [{"jid", jlib:jid_to_string(
				      {Headers#headers.room,
				       Headers#headers.host,
				       Nick})},
			    {"name", Nick}], []}
		  end,
		  ?DICT:to_list(Headers#headers.users)),
	    {result, UList, Headers};
	_ ->
	    {error, ?ERR_FORBIDDEN}
    end;

process_iq(_UserInfo, _Role, ?NS_CAPTCHA, get, _Lang, _SubEl, _Headers) ->
    {error, ?ERR_NOT_ALLOWED};

process_iq(_UserInfo, _Role, ?NS_CAPTCHA, set, _Lang, SubEl, Headers) ->
    case ejabberd_captcha:process_reply(SubEl) of
	ok ->
	    {result, [], Headers};
	_ ->
	    {error, ?ERR_NOT_ACCEPTABLE}
    end;

process_iq({not_in_room, _From}, _FAffiliation,_XMLNS, _Type, _Lang, _SubEl, _Headers)->
    {error, ?ERR_FORBIDDEN}.
    
process_presence(From, Packet, _Nick, _Lang, Headers)->
    Stanza = case {(Headers#headers.config)#config.allow_visitor_status,
                                   is_visitor(From, Headers)} of
                    {false, true} ->
                        mod_muc_room:strip_status(Packet);
                    _Allowed ->
                         Packet
                    end,
   {allow, Stanza, Headers}.

get_disco_item(User, Lang, Opts, nil)->
    get_disco_item(User, Lang, Opts, set_opts(Opts, #headers{}));
get_disco_item(#user{jid=Jid, role=FRole}, Lang, _Opts, Headers)->
    Config = Headers#headers.config,
    FAffiliation = mod_muc_room:get_affiliation(Jid, Headers),
    Tail =
	case (Config#config.public_list == true) orelse
	    (FRole /= none) orelse
	    (FAffiliation == admin) orelse
	    (FAffiliation == owner) of
	    true ->
		Desc = case Config#config.public of
			   true ->
			       "";
			   _ ->
			       translate:translate(Lang, "private, ")
		       end,
		Len = if Headers /= nil ->
		    ?DICT:fold(fun(_, _, Acc) -> Acc + 1 end, 0,
				 Headers#headers.users);
			true ->
			    0
		end,
		" (" ++ Desc ++ integer_to_list(Len) ++ ")";
	    _ ->
		" (n/a)"
	end,
    Reply = case (Config#config.public == true) orelse
		(FRole /= none) orelse
		(FAffiliation == admin) orelse
		(FAffiliation == owner) of
		true ->
		    {item, get_title(Headers) ++ Tail};
		_ ->
		    false
	    end,
    {ok, Reply, Headers}.
    
%get_disco_info({not_in_room, _From}, _Lang, #config{public=false})->
%    {error, ?ERR_FORBIDDEN};
get_disco_info(From, Lang, Opts, Headers)->
    {Len, NewHeaders}  = if Headers == nil->
        {0, set_opts(Opts, Headers)};
        true -> 
        {length(?DICT:to_list(Headers#headers.users)), Headers}
    end,
    Config = NewHeaders#headers.config,
    RoomDescription = Config#config.description,
    [{xmlelement, "identity",
	       [{"category", "conference"},
		{"type", "text"},
		{"name", get_title(Headers)}], []},
	      {xmlelement, "feature",
	       [{"var", ?NS_MUC}], []},
	      ?CONFIG_OPT_TO_FEATURE(Config#config.public,
				     "muc_public", "muc_hidden"),
	      ?CONFIG_OPT_TO_FEATURE(Config#config.persistent,
				     "muc_persistent", "muc_temporary"),
	      ?CONFIG_OPT_TO_FEATURE(Config#config.members_only,
				     "muc_membersonly", "muc_open"),
	      ?CONFIG_OPT_TO_FEATURE(Config#config.anonymous,
				     "muc_semianonymous", "muc_nonanonymous"),
	      ?CONFIG_OPT_TO_FEATURE(Config#config.moderated,
				     "muc_moderated", "muc_unmoderated"),
	      ?CONFIG_OPT_TO_FEATURE(Config#config.password_protected,
				     "muc_passwordprotected", "muc_unsecured"),
	     {xmlelement, "x", [{"xmlns", ?NS_XDATA}, {"type", "result"}],
      [?RFIELDT("hidden", "FORM_TYPE",
		"http://jabber.org/protocol/muc#roominfo"),
       ?RFIELD("Room description", "muc#roominfo_description",
	       RoomDescription),
       ?RFIELD("Number of occupants", "muc#roominfo_occupants",
	       integer_to_list(Len))
      ]}].
%%%%%%%%%%%
%% UTILS function


    
get_default_role(Affiliation, Headers)->
    case Affiliation of
	owner ->   moderator;
	admin ->   moderator;
	member ->  participant;
	outcast -> none;
	none ->
	    case (Headers#headers.config)#config.members_only of
		true ->
		    none;
		_ ->
		    case (Headers#headers.config)#config.members_by_default of
			true ->
			    participant;
			_ ->
			    visitor
		    end
	    end
    end.


can_change_subject(Role, Config) ->
    case (Config#headers.config)#config.allow_change_subj of
	true ->
	    (Role == moderator) orelse (Role == participant);
	_ ->
	    Role == moderator
    end.



is_visitor(Jid, Headers) ->
    mod_muc_room:get_role(Jid, Headers) =:= visitor.
    

-define(CASE_CONFIG_OPT(Opt),
	Opt -> Headers#headers{config = (Headers#headers.config)#config{Opt = Val}}).

set_opts([], Headers) -> Headers;
set_opts([{Opt, Val} | Opts], Headers) ->
    NH = case Opt of
	      ?CASE_CONFIG_OPT(title);
	      ?CASE_CONFIG_OPT(description);
	      ?CASE_CONFIG_OPT(allow_change_subj);
	      ?CASE_CONFIG_OPT(allow_query_users);
	      ?CASE_CONFIG_OPT(allow_private_messages);
	      ?CASE_CONFIG_OPT(allow_visitor_nickchange);
	      ?CASE_CONFIG_OPT(allow_visitor_status);
	      ?CASE_CONFIG_OPT(public);
	      ?CASE_CONFIG_OPT(public_list);
	      ?CASE_CONFIG_OPT(moderated);
	      ?CASE_CONFIG_OPT(persistent);
	      ?CASE_CONFIG_OPT(members_by_default);
	      ?CASE_CONFIG_OPT(members_only);
	      ?CASE_CONFIG_OPT(allow_user_invites);
	      ?CASE_CONFIG_OPT(password_protected);
	      ?CASE_CONFIG_OPT(captcha_protected);
	      ?CASE_CONFIG_OPT(password);
	      ?CASE_CONFIG_OPT(anonymous);
	      ?CASE_CONFIG_OPT(logging);
	      max_users ->
		  ServiceMaxUsers = mod_muc_room:get_service_max_users(Headers#headers.server_host),
		  MaxUsers = if
				 Val =< ServiceMaxUsers -> Val;
				 true -> ServiceMaxUsers
			     end,
		  Headers#headers{config = (Headers#headers.config)#config{max_users = MaxUsers}};
	      affiliations ->
		  Headers#headers{affiliations = ?DICT:from_list(Val)};
	      subject ->
		  Headers#headers{subject = Val};
	      subject_author ->
		  Headers#headers{subject_author = Val};
	      _ -> 
	        Config = Headers#headers.config,
	        Headers#headers{config = Config#config{custom = [{Opt, Val}| Config#config.custom]}}
	  end, 
    set_opts(Opts, NH).

-define(MAKE_CONFIG_OPT(Opt), {Opt, Config#config.Opt}).

make_opts(Headers) ->
    Config = Headers#headers.config,
    [
     ?MAKE_CONFIG_OPT(title),
     ?MAKE_CONFIG_OPT(description),
     ?MAKE_CONFIG_OPT(allow_change_subj),
     ?MAKE_CONFIG_OPT(allow_query_users),
     ?MAKE_CONFIG_OPT(allow_private_messages),
     ?MAKE_CONFIG_OPT(allow_visitor_status),
     ?MAKE_CONFIG_OPT(allow_visitor_nickchange),
     ?MAKE_CONFIG_OPT(public),
     ?MAKE_CONFIG_OPT(public_list),
     ?MAKE_CONFIG_OPT(moderated),
     ?MAKE_CONFIG_OPT(persistent),
     ?MAKE_CONFIG_OPT(max_users),
     ?MAKE_CONFIG_OPT(members_by_default),
     ?MAKE_CONFIG_OPT(members_only),
     ?MAKE_CONFIG_OPT(allow_user_invites),
     ?MAKE_CONFIG_OPT(password_protected),
     ?MAKE_CONFIG_OPT(captcha_protected),
     ?MAKE_CONFIG_OPT(password),
     ?MAKE_CONFIG_OPT(anonymous),
     ?MAKE_CONFIG_OPT(logging),
     {affiliations, ?DICT:to_list(Headers#headers.affiliations)},
     {subject, Headers#headers.subject},
     {subject_author, Headers#headers.subject_author}
     | Config#config.custom
    ].



get_default_room_maxusers(Headers) ->
    DefRoomOpts = gen_mod:get_module_opt(Headers#headers.server_host, mod_muc, default_room_options, []),
    H2 = set_opts(DefRoomOpts, Headers),
    get_max_users(H2).

get_config(#user{jid=From}, owner, Lang, Headers) ->
    {_AccessRoute, _AccessCreate, _AccessAdmin, AccessPersistent} = Headers#headers.access,
    ServiceMaxUsers = mod_muc_room:get_service_max_users(Headers),
    DefaultRoomMaxUsers = get_default_room_maxusers(Headers),
    Config = Headers#headers.config,
    {MaxUsersRoomInteger, MaxUsersRoomString} =
	case mod_muc_room:get_max_users(Headers) of
	    N when is_integer(N) ->
		{N, erlang:integer_to_list(N)};
	    _ -> {0, "none"}
	end,
    Res =
	[{xmlelement, "title", [],
	  [{xmlcdata, translate:translate(Lang, "Configuration for ") ++
	    jlib:jid_to_string(Headers#headers.jid)}]},
	 {xmlelement, "field", [{"type", "hidden"},
				{"var", "FORM_TYPE"}],
	  [{xmlelement, "value", [],
	    [{xmlcdata, "http://jabber.org/protocol/muc#roomconfig"}]}]},
	 ?STRINGXFIELD("Room title",
		       "muc#roomconfig_roomname",
		       Config#config.title),
	 ?STRINGXFIELD("Room description",
		       "muc#roomconfig_roomdesc",
		       Config#config.description)
	] ++
	 case acl:match_rule(Headers#headers.server_host, AccessPersistent, From) of
		allow ->
			[?BOOLXFIELD(
			 "Make room persistent",
			 "muc#roomconfig_persistentroom",
			 Config#config.persistent)];
		_ -> []
	 end ++ [
	 ?BOOLXFIELD("Make room public searchable",
		     "muc#roomconfig_publicroom",
		     Config#config.public),
	 ?BOOLXFIELD("Make participants list public",
		     "public_list",
		     Config#config.public_list),
	 ?BOOLXFIELD("Make room password protected",
		     "muc#roomconfig_passwordprotectedroom",
		     Config#config.password_protected),
	 ?PRIVATEXFIELD("Password",
			"muc#roomconfig_roomsecret",
			case Config#config.password_protected of
			    true -> Config#config.password;
			    false -> ""
			end),
	 {xmlelement, "field",
	  [{"type", "list-single"},
	   {"label", translate:translate(Lang, "Maximum Number of Occupants")},
	   {"var", "muc#roomconfig_maxusers"}],
	  [{xmlelement, "value", [], [{xmlcdata, MaxUsersRoomString}]}] ++
	  if
	      is_integer(ServiceMaxUsers) -> [];
	      true ->
		  [{xmlelement, "option",
		    [{"label", translate:translate(Lang, "No limit")}],
		    [{xmlelement, "value", [], [{xmlcdata, "none"}]}]}]
	  end ++
	  [{xmlelement, "option", [{"label", erlang:integer_to_list(N)}],
	    [{xmlelement, "value", [],
	      [{xmlcdata, erlang:integer_to_list(N)}]}]} ||
	      N <- lists:usort([ServiceMaxUsers, DefaultRoomMaxUsers, MaxUsersRoomInteger |
			       ?MAX_USERS_DEFAULT_LIST]), N =< ServiceMaxUsers]
	 },
	 {xmlelement, "field",
	  [{"type", "list-single"},
	   {"label", translate:translate(Lang, "Present real Jabber IDs to")},
	   {"var", "muc#roomconfig_whois"}],
	  [{xmlelement, "value", [], [{xmlcdata,
				       if Config#config.anonymous ->
					       "moderators";
					  true ->
					       "anyone"
				       end}]},
	   {xmlelement, "option", [{"label", translate:translate(Lang, "moderators only")}],
	    [{xmlelement, "value", [], [{xmlcdata, "moderators"}]}]},
	   {xmlelement, "option", [{"label", translate:translate(Lang, "anyone")}],
	    [{xmlelement, "value", [], [{xmlcdata, "anyone"}]}]}]},
	 ?BOOLXFIELD("Make room members-only",
		     "muc#roomconfig_membersonly",
		     Config#config.members_only),
	 ?BOOLXFIELD("Make room captcha protected",
		     "captcha_protected",
		     Config#config.captcha_protected),
	 ?BOOLXFIELD("Make room moderated",
		     "muc#roomconfig_moderatedroom",
		     Config#config.moderated),
	 ?BOOLXFIELD("Default users as participants",
		     "members_by_default",
		     Config#config.members_by_default),
	 ?BOOLXFIELD("Allow users to change subject",
		     "muc#roomconfig_changesubject",
		     Config#config.allow_change_subj),
	 ?BOOLXFIELD("Allow users to send private messages",
		     "allow_private_messages",
		     Config#config.allow_private_messages),
	 ?BOOLXFIELD("Allow users to query other users",
		     "allow_query_users",
		     Config#config.allow_query_users),
	 ?BOOLXFIELD("Allow users to send invites",
		     "muc#roomconfig_allowinvites",
		     Config#config.allow_user_invites),
	 ?BOOLXFIELD("Allow visitors to send status text in presence updates",
		     "muc#roomconfig_allowvisitorstatus",
		     Config#config.allow_visitor_status),
	 ?BOOLXFIELD("Allow visitors to change nickname",
		     "muc#roomconfig_allowvisitornickchange",
		     Config#config.allow_visitor_nickchange)
	] ++
	case mod_muc_log:check_access_log(
	       Headers#headers.server_host, From) of
	    allow ->
		[?BOOLXFIELD(
		    "Enable logging",
		    "muc#roomconfig_enablelogging",
		    Config#config.logging)];
	    _ -> []
	end,
    {result, [{xmlelement, "instructions", [],
	       [{xmlcdata,
		 translate:translate(
		   Lang, "You need an x:data capable client to configure room")}]},
	      {xmlelement, "x", [{"xmlns", ?NS_XDATA},
				 {"type", "form"}],
	       Res}],
     Headers};

get_config(_UserInfo, _Aff,Lang, _Headers) ->
    ErrText = "Owner privileges required",
	{error, ?ERRT_FORBIDDEN(Lang, ErrText)}.

set_config(#user{jid = From}, owner, XEl, _Lang, Headers) ->
    case {check_allowed_log_change(XEl, Headers, From),
		  check_allowed_persistent_change(XEl, Headers, From)} of
	{allow, allow} -> 
        XData = jlib:parse_xdata_submit(XEl),
        case XData of
	    invalid ->
	        ?DEBUG("set_config : invalid", []),
	        {error, ?ERR_BAD_REQUEST};
	    _ ->
	        case set_xoption(XData, Headers#headers.config) of
	    	Config when is_record(Config, config)->
                case {(Headers#headers.config)#config.members_only,
                       Config#config.members_only} of
                {false, true} ->
                    NSD1 = mod_muc_room:remove_nonmembers(Headers ),
                    {result, [], NSD1#headers{config=Config}};
                _ ->
                    {result, [], Headers#headers{config=Config}}
                 end;
	    	Err ->
	    	    ?DEBUG("set_config : ~p", [Err]),
	    	    Err
	        end
        end;
    Error -> 
		?DEBUG("set_config Error : ~p", [Error]),
		{error, ?ERR_BAD_REQUEST}
    end;
    
set_config(_UserInfo, _Aff, _XEl, Lang, _Headers) ->
    ErrText = "Owner privileges required",
	{error, ?ERRT_FORBIDDEN(Lang, ErrText)}.
	
-define(SET_BOOL_XOPT(Opt, Val),
	case Val of
	    "0" -> set_xoption(Opts, Config#config{Opt = false});
	    "false" -> set_xoption(Opts, Config#config{Opt = false});
	    "1" -> set_xoption(Opts, Config#config{Opt = true});
	    "true" -> set_xoption(Opts, Config#config{Opt = true});
	    _ -> {error, ?ERR_BAD_REQUEST}
	end).

-define(SET_NAT_XOPT(Opt, Val),
	case catch list_to_integer(Val) of
	    I when is_integer(I),
	           I > 0 ->
		set_xoption(Opts, Config#config{Opt = I});
	    _ ->
		{error, ?ERR_BAD_REQUEST}
	end).

-define(SET_STRING_XOPT(Opt, Val),
	set_xoption(Opts, Config#config{Opt = Val})).


set_xoption([], Config) ->
    Config;
set_xoption([{"muc#roomconfig_roomname", [Val]} | Opts], Config) ->
    ?SET_STRING_XOPT(title, Val);
set_xoption([{"muc#roomconfig_roomdesc", [Val]} | Opts], Config) ->
    ?SET_STRING_XOPT(description, Val);
set_xoption([{"muc#roomconfig_changesubject", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_change_subj, Val);
set_xoption([{"allow_query_users", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_query_users, Val);
set_xoption([{"allow_private_messages", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_private_messages, Val);
set_xoption([{"muc#roomconfig_allowvisitorstatus", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_visitor_status, Val);
set_xoption([{"muc#roomconfig_allowvisitornickchange", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_visitor_nickchange, Val);
set_xoption([{"muc#roomconfig_publicroom", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(public, Val);
set_xoption([{"public_list", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(public_list, Val);
set_xoption([{"muc#roomconfig_persistentroom", [Val]} | Opts], Config) ->
   ?SET_BOOL_XOPT(persistent, Val);
set_xoption([{"muc#roomconfig_moderatedroom", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(moderated, Val);
set_xoption([{"members_by_default", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(members_by_default, Val);
set_xoption([{"muc#roomconfig_membersonly", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(members_only, Val);
set_xoption([{"captcha_protected", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(captcha_protected, Val);
set_xoption([{"muc#roomconfig_allowinvites", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(allow_user_invites, Val);
set_xoption([{"muc#roomconfig_passwordprotectedroom", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(password_protected, Val);
set_xoption([{"muc#roomconfig_roomsecret", [Val]} | Opts], Config) ->
    ?SET_STRING_XOPT(password, Val);
set_xoption([{"anonymous", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(anonymous, Val);
set_xoption([{"muc#roomconfig_whois", [Val]} | Opts], Config) ->
    case Val of
	"moderators" ->
	    ?SET_BOOL_XOPT(anonymous, "1");
	"anyone" ->
	    ?SET_BOOL_XOPT(anonymous, "0");
	_ ->
	    {error, ?ERR_BAD_REQUEST}
    end;
set_xoption([{"muc#roomconfig_maxusers", [Val]} | Opts], Config) ->
    case Val of
	"none" ->
	    ?SET_STRING_XOPT(max_users, none);
	_ ->
	    ?SET_NAT_XOPT(max_users, Val)
    end;
set_xoption([{"muc#roomconfig_enablelogging", [Val]} | Opts], Config) ->
    ?SET_BOOL_XOPT(logging, Val);
set_xoption([{"FORM_TYPE", _} | Opts], Config) ->
    %% Ignore our FORM_TYPE
    set_xoption(Opts, Config);
set_xoption([_ | _Opts], _Config) ->
    {error, ?ERR_BAD_REQUEST}.



process_changed_ra(JID, affiliation, owner, _, Headers)  when (JID#jid.luser == "") -> Headers;
process_changed_ra(JID, role, none, Reason, Headers)->
    catch mod_muc_room:send_kickban_presence(
        JID, Reason, "307", Headers),
    mod_muc_room:set_role(JID, none, Headers);
process_changed_ra(JID, affiliation, none, Reason, Headers)->
    case (Headers#headers.config)#config.members_only of
	    true ->
			catch mod_muc_room:send_kickban_presence(
			 JID, Reason, "321", Headers),
			Headers1 = mod_muc_room:set_affiliation(JID, none, Headers),
			mod_muc_room:set_role(JID, none, Headers1);
		_ ->
			Headers1 = mod_muc_room:set_affiliation(JID, none, Headers),
			mod_muc_room:send_update_presence(JID,"", Headers1),
			mod_muc_room:set_affiliation(JID, none, Headers1),
			Headers1
	end;
process_changed_ra(JID, affiliation, outcast, Reason, Headers)->
    catch mod_muc_room:send_kickban_presence(
	    JID, Reason, "301", Headers),
    mod_muc_room:set_affiliation_and_reason(
	    JID, outcast, Reason,
	    mod_muc_room:set_role(JID, none, Headers));
	    
process_changed_ra(JID, affiliation, A, Reason, Headers) when
					   (A == admin) or (A == owner) ->
	Headers1 = mod_muc_room:set_affiliation_and_reason(JID, A, Reason, Headers),
	Headers2 = mod_muc_room:set_role(JID, moderator, Headers1),
	mod_muc_room:send_update_presence(JID, Reason, Headers2),
	Headers2;				       
    
process_changed_ra(JID, affiliation, member, Reason, Headers)->
    SD1 = mod_muc_room:set_affiliation_and_reason(
	    JID, member, Reason, Headers),
	SD2 = mod_muc_room:set_role(JID, participant, SD1),
	mod_muc_room:send_update_presence(JID, Reason, SD2),
	SD2;
process_changed_ra(JID, role, Role, Reason, Headers)->
    SD1 = mod_muc_room:set_role(JID, Role, Headers),
	catch mod_muc_room:send_new_presence(JID, Reason, SD1),
	SD1;
process_changed_ra(JID, affiliation, A, Reason, Headers)->
    SD1 = mod_muc_room:set_affiliation(JID, A, Headers),
	catch mod_muc_room:send_new_presence(JID, Reason, SD1),
	SD1.

can_change_ra(_FAffiliation, _FRole,
	      owner, _TRole,
	      affiliation, owner, owner, _Headers) ->
    %% A room owner tries to add as persistent owner a
    %% participant that is already owner because he is MUC admin
    true;
can_change_ra(_FAffiliation, _FRole,
	      TAffiliation, _TRole,
	      affiliation, Value, _ServiceAf, _Headers)
  when (TAffiliation == Value) ->
    nothing;
can_change_ra(_FAffiliation, _FRole,
	      _TAffiliation, TRole,
	      role, Value, _ServiceAf, _Headers)
  when (TRole == Value) ->
    nothing;
can_change_ra(FAffiliation, _FRole,
	      outcast, _TRole,
	      affiliation, none, _ServiceAf, _Headers)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      outcast, _TRole,
	      affiliation, member, _ServiceAf, _Headers)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(owner, _FRole,
	      outcast, _TRole,
	      affiliation, admin, _ServiceAf, _Headers) ->
    true;
can_change_ra(owner, _FRole,
	      outcast, _TRole,
	      affiliation, owner, _ServiceAf, _Headers) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      none, _TRole,
	      affiliation, outcast, _ServiceAf, _Headers)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      none, _TRole,
	      affiliation, member, _ServiceAf, _Headers)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(owner, _FRole,
	      none, _TRole,
	      affiliation, admin, _ServiceAf, _Headers) ->
    true;
can_change_ra(owner, _FRole,
	      none, _TRole,
	      affiliation, owner, _ServiceAf, _Headers) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      member, _TRole,
	      affiliation, outcast, _ServiceAf, _Headers)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      member, _TRole,
	      affiliation, none, _ServiceAf, _Headers)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(owner, _FRole,
	      member, _TRole,
	      affiliation, admin, _ServiceAf, _Headers) ->
    true;
can_change_ra(owner, _FRole,
	      member, _TRole,
	      affiliation, owner, _ServiceAf, _Headers) ->
    true;
can_change_ra(owner, _FRole,
	      admin, _TRole,
	      affiliation, _Affiliation, _ServiceAf, _Headers) ->
    true;
can_change_ra(owner, _FRole,
	      owner, _TRole,
	      affiliation, _Affiliation, _ServiceAf, _Headers) ->
    check_owner;
can_change_ra(_FAffiliation, _FRole,
	      _TAffiliation, _TRole,
	      affiliation, _Value, _ServiceAf, _Headers) ->
    false;
can_change_ra(_FAffiliation, moderator,
	      _TAffiliation, visitor,
	      role, none, _ServiceAf, _Headers) ->
    true;
can_change_ra(_FAffiliation, moderator,
	      _TAffiliation, visitor,
	      role, participant, _ServiceAf, _Headers) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      _TAffiliation, visitor,
	      role, moderator, _ServiceAf, _Headers)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(_FAffiliation, moderator,
	      _TAffiliation, participant,
	      role, none, _ServiceAf, _Headers) ->
    true;
can_change_ra(_FAffiliation, moderator,
	      _TAffiliation, participant,
	      role, visitor, _ServiceAf, _Headers) ->
    true;
can_change_ra(FAffiliation, _FRole,
	      _TAffiliation, participant,
	      role, moderator, _ServiceAf, _Headers)
  when (FAffiliation == owner) or (FAffiliation == admin) ->
    true;
can_change_ra(_FAffiliation, _FRole,
	      owner, moderator,
	      role, visitor, _ServiceAf, _Headers) ->
    false;
can_change_ra(owner, _FRole,
	      _TAffiliation, moderator,
	      role, visitor, _ServiceAf, _Headers) ->
    true;
can_change_ra(_FAffiliation, _FRole,
	      admin, moderator,
	      role, visitor, _ServiceAf, _Headers) ->
    false;
can_change_ra(admin, _FRole,
	      _TAffiliation, moderator,
	      role, visitor, _ServiceAf, _Headers) ->
    true;
can_change_ra(_FAffiliation, _FRole,
	      owner, moderator,
	      role, participant, _ServiceAf, _Headers) ->
    false;
can_change_ra(owner, _FRole,
	      _TAffiliation, moderator,
	      role, participant, _ServiceAf, _Headers) ->
    true;
can_change_ra(_FAffiliation, _FRole,
	      admin, moderator,
	      role, participant, _ServiceAf, _Headers) ->
    false;
can_change_ra(admin, _FRole,
	      _TAffiliation, moderator,
	      role, participant, _ServiceAf, _Headers) ->
    true;
can_change_ra(_FAffiliation, _FRole,
	      _TAffiliation, _TRole,
	      role, _Value, _ServiceAf, _Headers) ->
    false.


list_to_role(Role, _Headers) ->
    case Role of
	"moderator" ->   moderator;
	"participant" -> participant;
	"visitor" ->     visitor;
	"none" ->        none
    end.

list_to_affiliation(Affiliation, _Headers) ->
    case Affiliation of
	"owner" ->   owner;
	"admin" ->   admin;
	"member" ->  member;
	"outcast" -> outcast;
	"none" ->    none
    end.
 
check_password(owner, _Affiliation, _Els, _From, _StateData) ->
    %% Don't check pass if user is owner in MUC service (access_admin option)
    true;
check_password(_ServiceAffiliation, Affiliation, Els, From, Headers) ->
    case (Headers#headers.config)#config.password_protected of
	false ->
	    check_captcha(Affiliation, From, Headers);
	true ->
	    Pass = mod_muc_room:extract_password(Els),
	    case Pass of
		false ->
		    nopass;
		_ ->
		    case (Headers#headers.config)#config.password of
			Pass ->
			    true;
			_ ->
			    false
		    end
	    end
    end.

check_captcha(Affiliation, From, Headers) ->
    case (Headers#headers.config)#config.captcha_protected of
	true when Affiliation == none ->
	    case ?DICT:find(From, Headers#headers.robots) of
		{ok, passed} ->
		    true;
		_ ->
		    captcha_required
	    end;
	_ ->
	    true
    end.


