%% @doc Multi User Chatroom persistent storage on S3.
%% You need to specify the s3_bucket option in the ejabberd.cfg mod_muc options
%% And set {storage, s3_muc_storage}.
%% You'll need to have the <a href="http://github.com/cstar/erls3/">erls3 application</a> installed.
-module(s3_muc_storage).
-author('eric@ohmforce.com').

-behaviour(gen_muc_storage).


-include("ejabberd.hrl").
-include("jlib.hrl").
-include("mod_muc.hrl"). 
-export([init/3,
         store_room/5,
         restore_room/3,
         forget_room/3,
         fetch_room_names/2
         ]).



% @spec (Host, ServerHost, Opts)-> ok | {error, Reason}
% @doc called by {@link mod_muc:init/2}.
%
init(_Host, ServerHost, Opts)->
    ?DEBUG("init S3", []),
    Bucket = gen_mod:get_opt(s3_bucket, Opts, ServerHost),
    s3:start(),
    {ok, Buckets} = s3:list_buckets(),
    case lists:member(Bucket, Buckets) of 
        false ->
            s3:create_bucket(Bucket),
            ?INFO_MSG("S3 bucket ~s created", [Bucket]);
        true -> ok
    end,
    ets:new(s3_muc, [set, named_table, public]),
    ets:insert(s3_muc, {bucket, Bucket}),
    s3:start(),
    ok.
    
% @spec (Host,ServerHost, Name, Type, Opts)-> ok | {error, Reason}
% @doc Stores room.
% Called on configuration change when MUC is persistent.  
store_room(Host,ServerHost, Name, Type, Opts)->
    Bucket = get_bucket(),
    Key = build_key(Host, Name),
    Data = {Type, Opts},
    Res = s3:write_term(Bucket, Key, Data),
    Res.
    

% @spec (Host,ServerHost, Name)-> {Handler, Config}
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

% @spec (Host,ServerHost)-> [{Name, Host}]
% @doc Used for disco
% @deprecated
    
fetch_room_names(Host, ServerHost)->
    case catch s3:list_objects(get_bucket(),[{prefix,build_key(Host,"")}] ) of
    {'EXIT', Reason} ->
	    ?ERROR_MSG("~p", [Reason]),
	    [];
	{error, timeout} ->
	    ?ERROR_MSG("Timeout on S3", []),
	    [];
	{ok, Rooms} ->
	    lists:map(fun({object_info,{"Key",Key},_,_,_})->
            split_key(Key)
	    end, Rooms)
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