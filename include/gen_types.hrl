-ifndef(BRIDGE_GEN_TYPES).
-define(BRIDGE_GEN_TYPES, true).

-type hostname() :: atom() | string().
-type port_number() :: 0..65535.

-endif.
