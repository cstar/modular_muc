%%%----------------------------------------------------------------------
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

-define(MAX_USERS_DEFAULT, 200).
-define(MAX_USERS_DEFAULT_LIST,
	[5, 10, 20, 30, 50, 100, 200, 500, 1000, 2000, 5000]).
-define(SETS, gb_sets).
-define(DICT, dict).

%%% @type jid() = #jid{
%%%    user = string(),
%%%    server = string(),
%%%    resource = string(),
%%%    luser = string(),
%%%    lserver = string(),
%%%    lresource = string()}.
%%% @type ljid() = {User::string(), Server::string(), Resource::string()}.
%% @type headers() = [{atom(), any()}]       

%%% Useful defines :
-define(FEATURE(Var), {xmlelement, "feature", [{"var", Var}], []}).

-define(CONFIG_OPT_TO_FEATURE(Opt, Fiftrue, Fiffalse),
    case Opt of
	true ->
	    ?FEATURE(Fiftrue);
	false ->
	    ?FEATURE(Fiffalse)
    end).

%%%%%

-define(RFIELDT(Type, Var, Val),
	{xmlelement, "field", [{"type", Type}, {"var", Var}],
	 [{xmlelement, "value", [], [{xmlcdata, Val}]}]}).

-define(RFIELD(Label, Var, Val),
	{xmlelement, "field", [{"label", translate:translate(Lang, Label)},
			       {"var", Var}],
	 [{xmlelement, "value", [], [{xmlcdata, Val}]}]}).

%%%%

-define(XFIELD(Type, Label, Var, Val),
	{xmlelement, "field", [{"type", Type},
			       {"label", translate:translate(Lang, Label)},
			       {"var", Var}],
	 [{xmlelement, "value", [], [{xmlcdata, Val}]}]}).

-define(BOOLXFIELD(Label, Var, Val),
	?XFIELD("boolean", Label, Var,
		case Val of
		    true -> "1";
		    _ -> "0"
		end)).

-define(STRINGXFIELD(Label, Var, Val),
	?XFIELD("text-single", Label, Var, Val)).

-define(PRIVATEXFIELD(Label, Var, Val),
	?XFIELD("text-private", Label, Var, Val)).



%%%% Records

-record(lqueue, {queue, len, max}).

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
		

%% @type user() = #user{
%%          jid=jid(),
%%          nick=string(),
%%          role=atom(),
%%          last_presence=XMLMessage::string()} | {not_in_room, From::jid()}
%% @doc users are stored in this record in the mod_muc_room state.
%% If not in room, means that the client has not joined the room, but tries to message it.
%% Which may succeed, if the user is an admin.
-record(user, {jid,
	       nick,
	       role,
	       last_presence}).

-record(activity, {message_time = 0,
		   presence_time = 0,
		   message_shaper,
		   presence_shaper,
		   message,
		   presence}).

-record(state, {room,
		host,
		server_host,
		access,
		storage,
		handler,
		jid,
		config,
		users = ?DICT:new(),
		robots = ?DICT:new(),
		affiliations = ?DICT:new(),
		history,
		subject = "",
		subject_author = "",
		just_created = false,
		activity = treap:empty(),
		room_shaper,
		room_queue = queue:new()}).

-record(headers, {room,
		host,
		jid,
		server_host,
		access,
		storage,
		config,
		handler,
		users = ?DICT:new(),
		robots = ?DICT:new(),
		affiliations = ?DICT:new(),
		subject = "",
		subject_author = ""}).


-record(muc_online_users, {us,
			   room,
			   host}).
