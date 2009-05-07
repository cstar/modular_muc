%@doc Default storage implementation, in mnesia.
% A small refactor of what's done in current ejabberd distribution.
-module(muc_storage_default).
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
         
init(Host, _ServerHost, _Opts)->
    ?DEBUG("init muc_default", []),
    mnesia:create_table(muc_room,
		[{disc_copies, [node()]},
		 {attributes, record_info(fields, muc_room)}]),
    update_muc_room_table(Host),
    ok.
    
store_room(Host, _ServerHost,Type, Name, Opts)->
    ?DEBUG("store_room muc_default", []),
    F = fun() ->
		mnesia:write(#muc_room{name_host = {Name, Host}, type=Type,
				       opts = Opts})
	end,
    mnesia:transaction(F).

restore_room(Host,_ServerHost, Name)->
    ?DEBUG("restore_room muc_default", []),
   case catch mnesia:dirty_read(muc_room, {Name, Host}) of
	[#muc_room{opts = Opts, type=Type}] ->
	    {Type, Opts};
	_ ->
	    error
    end.

forget_room(Host, _ServerHost, Name) ->
    ?DEBUG("forget_room muc_default", []),
    F = fun() ->
		mnesia:delete({muc_room, {Name, Host}})
	end,
    mnesia:transaction(F).
    
    
fetch_room_names( Host, _ServerHost)->
    ?DEBUG("fetch_all_rooms muc_default", []),
    case catch mnesia:dirty_select(
		 muc_room, [{#muc_room{name_host = {'_', Host}, _ = '_'},
			     [],
			     ['$_']}]) of
	    {'EXIT', Reason} ->
	        ?ERROR_MSG("~p", [Reason]),
	        [];
	    Rs ->
	        lists:map(fun(#muc_room{name_host = NameHost})->
                NameHost
	        end, Rs)
    end.
    
    
%%%INTERNAL
update_muc_room_table(Host) ->
    Fields = record_info(fields, muc_room),
    case mnesia:table_info(muc_room, attributes) of
	Fields ->
	    ok;
	[name, opts] ->
	    ?INFO_MSG("Converting muc_room table from "
		      "{name, opts} format", []),
	    {atomic, ok} = mnesia:create_table(
			     mod_muc_tmp_table,
			     [{disc_only_copies, [node()]},
			      {type, bag},
			      {local_content, true},
			      {record_name, muc_room},
			      {attributes, record_info(fields, muc_room)}]),
	    mnesia:transform_table(muc_room, ignore, Fields),
	    F1 = fun() ->
			 mnesia:write_lock_table(mod_muc_tmp_table),
			 mnesia:foldl(
			   fun({muc_room, Name, Opts} = R, _) ->
				   mnesia:dirty_write(
				     mod_muc_tmp_table,
				     R#muc_room{name_host = {Name,Host}, type=muc_room_default, opts=Opts})
			   end, ok, muc_room)
		 end,
	    mnesia:transaction(F1),
	    mnesia:clear_table(muc_room),
	    F2 = fun() ->
			 mnesia:write_lock_table(muc_room),
			 mnesia:foldl(
			   fun(R, _) ->
				   mnesia:dirty_write(R)
			   end, ok, mod_muc_tmp_table)
		 end,
	    mnesia:transaction(F2),
	    mnesia:delete_table(mod_muc_tmp_table);
	[name_host, opts] ->
	    ?INFO_MSG("Converting muc_room table from "
		      "{name_host, opts} format", []),
	    {atomic, ok} = mnesia:create_table(
			     mod_muc_tmp_table,
			     [{disc_only_copies, [node()]},
			      {type, bag},
			      {local_content, true},
			      {record_name, muc_room},
			      {attributes, record_info(fields, muc_room)}]),
	    mnesia:transform_table(muc_room, ignore, Fields),
	    F1 = fun() ->
			 mnesia:write_lock_table(mod_muc_tmp_table),
			 mnesia:foldl(
			   fun({muc_room, Name_Host, Opts} = R, _) ->
				   mnesia:dirty_write(
				     mod_muc_tmp_table,
				     R#muc_room{name_host = Name_Host, type=muc_room_default, opts= Opts})
			   end, ok, muc_room)
		 end,
	    mnesia:transaction(F1),
	    mnesia:clear_table(muc_room),
	    F2 = fun() ->
			 mnesia:write_lock_table(muc_room),
			 mnesia:foldl(
			   fun(R, _) ->
				   mnesia:dirty_write(R)
			   end, ok, mod_muc_tmp_table)
		 end,
	    mnesia:transaction(F2),
	    mnesia:delete_table(mod_muc_tmp_table);
	_ ->
	    ?INFO_MSG("Recreating muc_room table", []),
	    mnesia:transform_table(muc_room, ignore, Fields)
    end.