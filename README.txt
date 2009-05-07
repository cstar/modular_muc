Modular muc implementation for maximum flexibility.
Based on ejabberd's mod_muc.

See overview.edoc for more info

It is still very very rough. I'd love input/patches from you guys

BUILDING :

Run rake twice : a first time for specifying location of erlang and ejabberd in generated erlang_config.rb
second time for actually building.

DOCUMENTATION :
There a bit of edoc, rake edoc to get it built.

INSTALL :
rake install will do the trick

TODO : 
- remove build warnings.
- refine API

LICENSE :
GPL as mod_muc on which the code is based on.