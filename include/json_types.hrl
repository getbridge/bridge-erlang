-type json_key() :: atom()
                  | binary().

-type json_obj() :: {proplist(json_key(), json())}.

-type json() :: json_obj()
              | [json()]
              | json_key()
              | number().

-type proplist(K, V) :: [{K, V}].
