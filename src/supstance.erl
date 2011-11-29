%% @doc Simplified child specifications generation and prevalidation.
%% Module implements a set of self-descriptive routines which do:
%% <ll>
%% <li>make up valid supervisor childspecs based on three arguments only;</li>
%% <li>prevalidate childspecs been made thus throwing informative exceptions if something is wrong.</li>
%% </ll>
-module(supstance).

-export([

    childspec/4,
    permanent/3,
    transient/3,
    temporary/3,
    supervisor/3,

    options/1
    
]).

-define(DEFAULT_TIMEOUT, 5000).

-spec permanent(Specification, RegistrationType, Options) -> supervisor:child_spec() when
    Specification    :: atom() | {term(), atom()} | {term(), atom(), atom()},
    RegistrationType :: none | local | global,
    Options          :: inherit | global | term().

permanent(Spec, RegType, Options) -> childspec(permanent, Spec, RegType, Options).

-spec transient(Specification, RegistrationType, Options) -> supervisor:child_spec() when
    Specification    :: atom() | {term(), atom()} | {term(), atom(), atom()},
    RegistrationType :: none | local | global,
    Options          :: inherit | global | term().

transient(Spec, RegType, Options) -> childspec(transient, Spec, RegType, Options).

-spec temporary(Specification, RegistrationType, Options) -> supervisor:child_spec() when
    Specification    :: atom() | {term(), atom()} | {term(), atom(), atom()},
    RegistrationType :: none | local | global,
    Options          :: inherit | global | term().

temporary(Spec, RegType, Options) -> childspec(temporary, Spec, RegType, Options).

-spec supervisor(Specification, RegistrationType, Options) -> supervisor:child_spec() when
    Specification    :: atom() | {term(), atom()} | {term(), atom(), atom()},
    RegistrationType :: none | local | global,
    Options          :: inherit | global | term().

supervisor(Spec, RegType, Options) -> childspec(supervisor, Spec, RegType, Options).

-spec childspec(Role, Specification, RegistrationType, Options) -> supervisor:child_spec() when
    Role             :: permanent | transient | temporary | supervisor,
    Specification    :: atom() | {term(), atom()} | {term(), atom(), atom()},
    RegistrationType :: none | local | global,
    Options          :: inherit | global | term().

childspec(Role, Spec, RegType, Options) ->
    ok = valid_regtype(RegType),
    {Name, Entry, Deps} = entry(Spec, {RegType, Options}),
    {Name, Entry, restart_mode(Role), kill_mode(Role), role(Role), Deps}.

%% @private
entry({Name, Module, Entry}, {RegType, Options}) ->
    ok = valid_name(RegType, Name),
    ok = valid_entry(Module, Entry, RegType),
    {Name, {Module, Entry, arguments(RegType, Name, options(Options, Name))}, [Module]};

entry({Name, Module}, Options) ->
    entry({Name, Module, start_link}, Options);

entry(Module, Options) when is_atom(Module) ->
    entry({Module, Module, start_link}, Options);

entry(Something, _) ->
    throw({invalid_spec, Something}).

%% @private
arguments(none, _, Args) -> [Args];
arguments(Type, Name, Args) -> [{Type, Name}, Args].

%% @private
role(supervisor) -> supervisor;
role(_) -> worker.

%% @private
restart_mode(supervisor) -> permanent;
restart_mode(Mode) -> Mode.

%% @private
kill_mode(supervisor) -> infinity;
kill_mode(temporary) -> brutal_kill;
kill_mode(_) -> ?DEFAULT_TIMEOUT.

-spec options(Whose) -> proplists:proplist() when
    Whose :: {inherit, atom()} | global.

options({inherit, Name}) ->
    deep_props:get(Name, options(global), []);

options(global) ->
    application:get_all_env().

%% @private
options(inherit, Name) ->
    options({inherit, Name});

options(global, _) ->
    options(global);

options(Other, _) ->
    Other.

%% @private
valid_regtype(local)  -> ok;
valid_regtype(global) -> ok;
valid_regtype(none)   -> ok;
valid_regtype(Other)  -> throw({invalid_regtype, Other}).

%% @private
valid_entry(Module, Entry, Reg) ->
    try
        validate_entry(Module, Entry, Reg)
    catch _:Error ->
        throw({invalid_module_entry, {Module, Entry}, Error})
    end.

%% @private
validate_entry(Module, Entry, Reg) when is_atom(Module), is_atom(Entry) ->
    case code:ensure_loaded(Module) of
        {module, Module}          -> ok;
        {error, embedded}         -> ok;
        {error, sticky_directory} -> ok;
        {error, Error}            -> throw({loading_failed, Error})
    end,
    ok = validate_export(Module, Entry, Reg);

validate_entry(_, _, _) -> 
    throw(invalid_name).

%% @private
validate_export(Module, Entry, Reg) ->
    Info = Module:module_info(),
    case deep_props:extract([exports, Entry], Info, no_export) of
        {no_export, _} -> 
            throw(no_export);
        {Arities, _} when is_list(Arities) -> 
            Results = lists:map(fun (A) -> valid_arity(Reg, A) end, Arities),
            case lists:any(fun (ok) -> true; (_) -> false end, Results) of
                true -> ok;
                _    -> throw({invalid_entry_arity, Arities})
            end;
        {Arity, _} when is_integer(Arity) -> 
            case valid_arity(Reg, Arity) of
                ok -> ok;
                _  -> throw({invalid_entry_arity, Arity})
            end
    end.

%% @private
valid_arity(local, 2)     -> ok;
valid_arity(global, 2)    -> ok;
valid_arity(none, 1)      -> ok;
valid_arity(_, _)         -> error.

%% @private
valid_name(local, Name) when is_atom(Name) -> 
    ok;
valid_name(local, Other) -> 
    throw({invalid_local_name, Other});
valid_name(_, _) -> 
    ok.

%% Tests

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-compile([export_all]).

start(_Opts, _, _) -> 
    ok.

start_link(_Opts) -> 
    ok.

start_link(_Name, _Opts) -> 
    ok.

spec_test() ->
    Result = permanent(?MODULE, local, []),
    ?assertEqual({?MODULE, {?MODULE, start_link, [{local, ?MODULE}, []]}, permanent, 5000, worker, [?MODULE]}, Result).

spec_nameless_test() ->
    Result = permanent(?MODULE, none, []),
    ?assertEqual({?MODULE, {?MODULE, start_link, [[]]}, permanent, 5000, worker, [?MODULE]}, Result).

spec_temp_test() ->
    Result = temporary(?MODULE, none, []),
    ?assertEqual({?MODULE, {?MODULE, start_link, [[]]}, temporary, brutal_kill, worker, [?MODULE]}, Result).

spec_sup_test() ->
    Result = supervisor(?MODULE, local, []),
    ?assertEqual({?MODULE, {?MODULE, start_link, [{local, ?MODULE}, []]}, permanent, infinity, supervisor, [?MODULE]}, Result).

regtype_test() ->
    ?assertException(_, {invalid_regtype, wow}, supervisor(?MODULE, wow, [])).

failname_test() ->
    ?assertException(_, {invalid_local_name, {fail, name}}, supervisor({{fail, name}, ?MODULE}, local, [])).

nomodule_test() ->
    ?assertException(_, {invalid_module_entry, {'__no_module', start_link}, {loading_failed, nofile}}, supervisor('__no_module', local, [])).

noexport_test() ->
    ?assertException(_, {invalid_module_entry, {lists, start_link}, no_export}, supervisor(lists, local, [])).

mismatched_export_test() ->
    ?assertException(_, {invalid_module_entry, {?MODULE, start}, {invalid_entry_arity, 3}}, supervisor({?MODULE, ?MODULE, start}, local, [])).

-endif.
