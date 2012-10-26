
-record(pickle_global,
        {module :: binary(),                    % module name, eg "__builtin__"
         name:: binary()}).                     % obj name, eg "filter"

-record(pickle_object,
        {class :: #pickle_global{},        % class
         new_args :: tuple(),              % args for __new__()
         state :: dict() | tuple()}).      % __dict__ or args for __setstate__()

-record(pickle_unicode,
        {value :: binary()}).
