-module(gen_muc_handler).
-author('eric@ohmforce.com').

-export([behaviour_info/1]).

behaviour_info(callbacks) ->
    [{process_groupchat_message,5},
         {process_private_message,4},
         {process_iq,7},
         {process_presence,5},
         {process_user_iq,5},
         {get_config,4},
         {set_config,5},
         {init,2},
         {init,4},
         {process_changed_ra,5},
         {can_join,7},
         {user_leaving, 4},
         {can_change_ra,8},
         {can_invite,6},
         {is_persistent,1},
         {is_anonymous,1},
         {get_max_users,1},
         {can_get_affiliations,3},
         {allow_nick_change,2},
         {should_log,1},
         {can_get_full_jids,2},
         {handle_info,2},
         {handle_sync_event,3},
         {list_to_role,2},
         {list_to_affiliation,2},
         {get_disco_info, 2}];
    
behaviour_info(_Other) ->
    undefined.
    