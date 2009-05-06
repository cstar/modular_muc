%% @doc Multi User Chatroom persistent storage on S3.
%% You need to specify the s3_bucket option in the ejabberd.cfg mod_muc options
%% And set {storage, s3_muc_storage}.
%% You'll need to have the <a href="http://github.com/cstar/erls3/">erls3 application</a> installed.
-module(s3_muc_storage).
-author('eric@ohmforce.com').

-behaviour(gen_muc_storage).

-record(muc_room, {name_host, type, opts}).
-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_muc_room.hrl"). 
-export([init/3,
         store_room/5,
         restore_room/3,
         forget_room/3,
         fetch_all_rooms/2
         ]).



% @spec (Host, ServerHost, Opts)-> ok | {error, Reason}
% @doc called by {@link mod_muc:init/2}.
%
init(_Host, _ServerHost, Opts)->
    ?DEBUG("init S3", []),
    case gen_mod:get_opt(s3_bucket, Opts, none) of
        none -> {error, "s3_bucket option not set in ejabberd.cfg for mod_muc"};
        B ->
            ets:new(s3_muc, [set, named_table, public]),
            ets:insert(s3_muc, {bucket, B}),
            s3:start(),
            ok
    end.
% @spec (Host,ServerHost, Name, Type, Opts)-> ok | {error, Reason}
% @doc Stores room.
% Called on configuration change when MUC is persistent.  
store_room(Host,ServerHost, Name, Type, Opts)->
    ?DEBUG("store_room S3", []),
    s3:write_object(get_bucket(), build_key(Host, Name), term_to_binary({Type, Opts}), "application/erlang").

% @spec (Host,ServerHost, Name)-> Config
% @doc restores room from storage
restore_room(Host,ServerHost, Name)->
    ?DEBUG("restore_room S3", []),
    case s3:read_object(get_bucket(), build_key(Host, Name)) of
     {ok, {Conf, _H}}->
      binary_to_term(list_to_binary(Conf));
     _ -> 
      error
    end.
% @spec (Host, ServerHost, Name)-> ok
% @doc removes room from storage
forget_room(Host, ServerHost, Name) ->
    ?DEBUG("forget_room muc_s3", []),
     s3:delete_object(get_bucket(), build_key(Host, Name)).

% @spec (ServerHost, Host)-> [MucRoom]
%   MucRoom = #muc_room{}
% @doc Used for disco
fetch_all_rooms(ServerHost, Host)->
    ?DEBUG("fetch_all_rooms muc_s3", []),
    case s3:get_objects(get_bucket(),[{prefix,"muc@"++Host}] ) of
	{'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]),
	    [];
	{error, timeout} ->
	    ?ERROR_MSG("Timeout on S3", []),
	    [];
	{ok, Rs}->
	    lists:map(fun({Key, Value})->
	        #muc_room{name_host = split_key(Key), opts=binary_to_term(list_to_binary(Value))}
	    end, Rs)
	end.
    
    
%%%INTERNAL
get_bucket()->
     [{bucket, Bucket}] = ets:lookup(s3_muc, bucket),
     Bucket.
     
build_key(Host, Name)->
    "muc@"++Host++"@"++Name.

split_key(Key)->
    [_, Host, Name] = string:tokens(Key, "@"),
    {Name, Host}.