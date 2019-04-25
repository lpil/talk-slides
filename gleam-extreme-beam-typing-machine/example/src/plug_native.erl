-module(plug_native).

-export([path_info/1, method/1]).

path_info(Conn) ->
    #{path_info := PathInfo} = Conn,
    PathInfo.

method(Conn) ->
    #{method := Method} = Conn,
    case Method of
        <<"POST">> -> post;
        <<"PATCH">> -> patch;
        <<"DELETE">> -> delete;
        <<"OPTIONS">> -> options;
        _ -> get
    end.
