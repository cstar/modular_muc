-module(minimal_muc).
-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_muc_room.hrl"). 
-author('eric@ohmforce.com').
-behaviour(gen_muc_handler).
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
         user_leaving/4,
         room_destroyed/2,
         process_changed_ra/5,
         can_join/7,
         can_change_ra/8,
         can_invite/6,
         is_persistent/1,
         is_anonymous/1,
         get_max_users/1,
         can_get_affiliations/3,
         allow_nick_change/2,
         should_log/1,
         can_get_full_jids/2,
         handle_info/2,
         handle_sync_event/3,
         list_to_role/2,
         list_to_affiliation/2,
         get_disco_info/4,
         get_disco_item/4]).

%% @spec (DefRoomOpts, Creator, Nick::string(), Headers)->
%%        {result, ok, Headers} | {error, Why, Headers}
%%  DefRoomOpts = [{atom(), any()}]  
%%  Creator = mod_muc:jid() 
%%  Headers = headers()
%% @doc Called upon new room creation. 
%%  DefRoomOpts : are default options as defined in the ejabberd config file.
%%  Creator : Creator of the room
init(DefRoomOpts,Creator, Nick,Headers)->{result, ok, Headers}.

%% @doc Called upon room reloading (if room is persistent)
%% @spec (Opts,Headers)->
%%        {result, ok, Headers} | {error, Why, Headers}
%%  Opts = [{atom(), any()}]
%%  Headers = headers()
%%  
init(Opts,Headers)->{result, ok, Headers}.

% Stanza processing
%% @spec (From, ToNick, Packet,Headers)->
%%      {allow, Packet2, Headers} | {deny, ErrorMessage, Headers}
%%  Packet = XMLContent::string() 
%%  Packet2 = XMLContent::string() 
%%  Headers = headers()
%% @doc Processes private messages from a user to a specific nick.
%% It is possible to block message delivery or modify message before delivery
process_private_message(From, ToNick, Packet,Headers)->{allow, Packet}.

%% @spec (From, Packet, FromNick, Role,Headers)->
%%      {allow, Packet2, Headers} | {deny, ErrorMessage, Headers} | {filter, {Packet2, Fun}, Headers} | {drop, Packet2, Headers}
%%  Packet = XMLContent::string() 
%%  Packet2 = XMLContent::string() 
%%  ErrorMessage  = string()
%%  Headers = headers()
%%  Fun = fun(FromNick, Role, Info)
%%  FromNick = string()
%%  Role = atom()
%%  Info = term()
%%
%% @doc filters groupchat message. 
%% Possible options are :
%% <ul>
%%  <li>allow : message is delivered, and possibly modified</li>
%%  <li>deny : message is not delivered, and ErrorMessage is returned as a Forbidden error</li>
%%  <li>drop : message is silently dropped</li>
%%  <li>filter : Fun is used to decide if message should be delivered per recipient, should return true or false</li>
%%</ul>
process_groupchat_message(From, Packet, FromNick, Role,Headers)->{allow, Packet}.


%% @spec (From, ToNick, Lang, Packet,Headers)->
%%      {allow, Packet2, Headers} | {deny, ErrorMessage, Headers}
%%  Packet = XMLContent::string() 
%%  Packet2 = XMLContent::string() 
%%  Headers = headers()
%%  Lang = string()
%% @doc Processes iq stanzas from a user to a specific nick.
%% It is possible to block iq delivery or modify iq before delivery
%% get, set, result and errors are filtered.
process_user_iq(From, ToNick, Lang, Packet,Headers)->{allow, Packet}.

%% @spec (UserInfo, FAffiliation, XMLNS, Type, Lang, SubEl,Headers)->
%%      {result, SubEl2, Headers} | {error, ErrorMessage, Headers}
%%  SubEl = XMLContent::string() 
%%  SubEl2 = XMLContent::string() 
%%  UserInfo=user()
%%  FAffiliation=atom()
%%  XMLNS=string()
%%  Type = get | set
%%  Headers = headers()
%%  Lang = string()
%% @doc Processes iq stanzas from a user to chatroom
%% This function is called only if the default mod_muc iq do not correspond. It is not possible to override default iq, such as ?NS_MUC_ADMIN or ?NS_MUC_OWNER
%% Chatroom configuration is handled by {@link minimal_muc:get_config/4 } and {@link minimal_muc:set_config/5}
process_iq(UserInfo, FAffiliation, XMLNS, Type, Lang, SubEl,Headers)->{error, ?ERR_NOT_ALLOWED}.

%% @spec (From::jid(), Packet, Nick::string(),Lang::string(),Headers::headers())->
%%      {allow, Packet2, Headers} | {deny, ErrorMessage, Headers}
%%  Packet = XMLContent::string() 
%%  Packet2 = XMLContent::string() 
%%  Headers = headers()
%% @doc Processes presence sent to the room
%% It is possible to block presence delivery or modify presence before delivery
process_presence(From, Packet, Nick,Lang,Headers)->{allow, Packet}.

get_disco_item(User, Lang, Opts, #headers{subject=Subject}=Headers)->
    {item, Subject}.
get_disco_info(From, Lang, Opts, Headers)->
    Title = if Headers =:= nil ->
            proplists:get_value(teaser, Opts);
        true ->
            Headers#headers.subject
    end,
    [{xmlelement, "identity",
	       [{"category", "conference"},
		{"type", "text"},
		{"name", "Chatroom"}], []},
	      {xmlelement, "feature",
	       [{"var", ?NS_MUC}], []}].

% FSM Processing
handle_sync_event({get_disco_item, JID, Lang}, _From, Headers)->
    Len = ?DICT:fold(fun(_, _, Acc) -> Acc + 1 end, 0,
				 Headers#headers.users),
	Tail = " (" ++ integer_to_list(Len) ++ ")",
    {ok, {item, Headers#headers.room ++ Tail}, Headers};
    
handle_sync_event(Event, From,Headers)->ok.
handle_info(Info,Headers)->ok.

%Config management
set_config(UserInfo, Aff, XEl, Lang,Headers)->{result, [], Headers}.
get_config(UserInfo, Affiliation, Lang, Headers)->{result,[], Headers}.
process_changed_ra(TJID, Type, Value, Reason,Headers)->Headers.
is_persistent(Headers)-> false.
is_anonymous(Headers)-> true.
should_log(Headers)-> false.
get_max_users(Headers)-> 100.
    
%Rights Management
%% @spec (UserInfo::user(), FAffiliation::atom(), Headers::headers())-> true|false
%% @doc Can the user retrieve the affiliations list ?
can_get_affiliations(UserInfo, FAffiliation, Headers)-> false.

%% @spec (From::jid(), Nick::string(), Affiliation::atom(), ServiceAffiliation::atom(), Lang::string(), Packet, Headers::headers())-> 
%%      {true, Role}|{false, Reason}
%%      Packet = XMLContent::string() 
%% @doc Deciding if the user can join the room.
%%  Affiliation is the affiliation specific to the room.
%%  ServiceAffiliation is specified in ejabberd's configuration file.
%%  Return true and given
can_join(From, Nick, Affiliation, ServiceAffiliation, Lang, Packet, Headers)-> {true, admin}.
can_invite(From, FAffiliation,JID, InviteEl, Lang,Headers)->{false, [], Headers}.

% @spec (From::jid(), Nick::string(), Reason::string(), Headers::headers())->
%   ok
% @doc Hook called when user disconnects. Return value not used.
user_leaving(From, Nick, Reason, Headers)->
    ok.

%% @spec (UserInfo::user(), Headers::headers())-> true|false
%% @doc Can the user see the real JID ?
can_get_full_jids(User,Headers)-> false.
%% @spec (From::jid(), Headers::headers())-> true|false
%% @doc Can user change nickname ?
allow_nick_change(From,Headers)-> false.
%% @spec (FAffiliation::atom(), FRole::atom(),
%%	      TAffiliation::atom(), TRole::atom(),
%%	      Type, Value, ServiceAf::atom(), Headers::headers()) -> true | false | check_owner | nothing
%%      Type = affiliation | role
%%      Value = atom()
%% @doc Rules checking is user can change affiliations or roles.
%% FAffiliation and FRole are the role/affiliation of the user operating the change
%% TAffiliation and TRole are the role/affiliation of the recipient
%% Value is the new value for the Type (role or affiliation)
%% ServiceAf is the ServiceAffiliation of the recipient
%% returns true or false. Also returns nothing is the role/affiliation has not changed.
%% Can also return check_owner (TBD : still not sure why it's necessary)
can_change_ra(FAffiliation, FRole,
	      TAffiliation, TRole,
	      Type, Value, ServiceAf, Headers)-> false.

%% @spec (StrAff::string(), Headers::headers)->atom()
%% @doc Convert string Affiliation (from a stanza) to its atom form (for management).
list_to_affiliation(StrAff,Headers)-> list_to_atom(StrAff).
%% @spec (StrRole::string(), Headers::headers)->atom()
%% @doc Convert string Role (from a stanza) to its atom form (for management).
list_to_role(StrRole,Headers)-> list_to_atom(StrRole).

%% @spec (Reason::atom(), Headers::headers)->atom()
%% @doc Convert string Affiliation (from a stanza) to its atom form (for management).
room_destroyed(Reason, Headers)->
    ok.

