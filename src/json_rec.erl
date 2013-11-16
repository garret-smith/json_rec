%%%-------------------------------------------------------------------
%%% @author Justin Kirby <jkirby@voalte.com>
%%% @copyright (C) 2011,
%%% @doc
%%% Assuming a record  of `-record(simple, {one, two})' in mod_fake
%%% Usage example:
%%% ```
%%%   Rec = mod_fake:new(<<"simple">>),
%%%   Json = mochijson2:decode("{'one':1,'two':2}"),
%%%   SimpleRec = json_rec:to_rec(Json,mod_fake,Rec)
%%%
%%% '''
%%%
%%% The above code will take the json and transform it into the
%%% specified record. Trying to match the field of the record with the
%%% key in the json. If a match fails, then json_rec will fall back to
%%% using proplists
%%%
%%% The module MUST export module:new/1. new/1 should take a binary and return a record. Example:
%%% ```
%%% -module(mod_fake).
%%% -export([new/1]).
%%% -record(simple, {one,two}).
%%% new(<<"simple">>) -> #simple{};
%%% new(_) -> undefined.
%%% '''
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(json_rec).

-export([
         to_rec/3,
         to_json/2,
         to_json/3
        ]).

-include("json_rec_types.hrl").

%% note: I am using tuple() for record, since this is a generic record
-spec to_json(Record :: tuple(), Module :: [atom()]) -> {struct, proplist()};
             (Record :: tuple(), Module :: atom())  -> {struct, proplist()}.

to_json(Record, Module, Discriminator) when is_list(Module) ->
    Fields = module_rec_fields(Module,Record),
    Pl = rec_keys(Fields, Record, Module, Discriminator, []),
    case is_binary(Discriminator) of
        true -> {struct, [{Discriminator, atom_to_binary(element(1,Record))} | Pl]};
        false -> {struct, Pl}
    end;

to_json(Record, Module, Discriminator) ->
    to_json(Record, [Module], Discriminator).

to_json(Record, Module) -> to_json(Record, Module, none).

rec_keys([], _Record, _Module, _Discriminator, Acc) -> Acc;
rec_keys([Field|Rest],Record,Module,Discriminator, Acc) ->
    Value = module_get(Module, Field, Record),
    Key = atom_to_binary(Field),
    JsonValue = field_value(Value,Module,Discriminator,[]),
    rec_keys(Rest, Record, Module, Discriminator, [{Key,JsonValue}|Acc]).

field_value(Value, Module, Discriminator, _Acc) when is_tuple(Value) ->
    case module_has_rec(Module, Value, false) of
        false ->
            Value;
        _M when is_atom(_M) ->
            to_json(Value,Module,Discriminator)
    end;
field_value(Value, _Module, _Discriminator, _Acc) when is_atom(Value) ->
    atom_to_binary(Value);

field_value([],_Module, _Discriminator, Acc)  -> lists:reverse(Acc);
field_value([{_,_}|_] = Pl, Module, Discriminator, Acc) ->
    %% it is a proplist, make it a dict
    {struct, [{Key, Value} || {Key, V2} <- Pl,
                          begin
                              Value = field_value(V2, Module, Discriminator, Acc),
                              true
                          end]};

field_value([Value|Rest], Module, none, Acc) ->
    NewValue = case field_value(Value,Module,none,[]) of
                   IsRec when is_tuple(IsRec),
                              is_atom(element(1,Value)) ->
                       %% this returned a record, so get the first
                       %% element from the rec tuple and do: {struct,
                       %% atom
                       {struct, [{atom_to_binary(element(1,Value)),IsRec}]};
                   %% IsTuple when is_tuple(IsTuple) ->
                   %%     tuple_to_list(IsTuple);
                   NotRec ->
                       NotRec
               end,
    field_value(Rest, Module,none,[NewValue|Acc]);
field_value([Value|Rest], Module, Discriminator, Acc) ->
    field_value(Rest, Module,Discriminator,[field_value(Value, Module, Discriminator,[])|Acc]);
field_value(Value,_Module,_Discriminator,_Acc) ->
    Value.




%% @spec to_rec(_Json, Module, Record) -> tuple()
%% @doc
%% Take the result from mochijson2:decode/1 and transform it into a
%% record, or proplist.
%%
%% _Json MUST the result of mochijson2:decode/1.
%% Module is a module that refers to a specific module which exports new/1.
%% Rec is the initial empty record #record_name{} or `module:new(<<"record_name">>)'
%%
%% NOTE: it is up to you to export and define module:new/1
-spec to_rec(_Json :: json_dict(), Module :: atom() | [atom()], undefined) ->
                    proplist();
            (_Json :: json_dict(), Module :: atom() | [atom()], Rec :: tuple() ) ->
                    Rec :: tuple().

to_rec({struct, Pl} = _Json, Module, undefined) when is_list(Module) ->
    pl(Pl, Module, []);
to_rec({struct, Pl} = _Json, Module, undefined) ->
    pl(Pl, [Module], []);

to_rec({struct, Pl} = _Json, Module, Discriminators) when is_list(Module), is_list(Discriminators) ->
    case has_discriminator(Pl, Module, Discriminators) of
        false -> pl(Pl, Module, Discriminators);
        {RecType, Pl2} ->
            case module_new(Module, RecType, undefined) of
                undefined -> pl(Pl, Module, Discriminators);
                Rec -> keys_rec(Pl2, Module, Discriminators, Rec)
            end
    end;
to_rec(Json, Module, Discriminator) when is_binary(Discriminator) ->
    to_rec(Json, Module, [Discriminator]);

to_rec({struct, Pl} = _Json, Module, Rec) when is_list(Module), is_tuple(Rec) ->
    keys_rec(Pl, Module, [], Rec);
to_rec(Json, Module, Rec) ->
    to_rec(Json, [Module], Rec).

keys_rec([], _Module, _Discriminators, Rec) -> Rec;
keys_rec([{Key, {struct, _Pl} = Val}|Rest], Module, Discriminators, Rec) when length(Discriminators) > 0 ->
    Field = binary_to_atom(Key),
    Value = to_rec(Val, Module, Discriminators),
    UpRec = module_set(Module, {Field, Value}, Rec),
    keys_rec(Rest, Module, Discriminators, UpRec);
keys_rec([{Key, {struct, Pl}}|Rest], Module, Discriminators, Rec) ->
    Field = binary_to_atom(Key),
    Value = case module_new(Module, Key, undefined) of
        undefined ->
            %% this is not a sub record, so just pl it
            pl(Pl,Module,Discriminators);
        SubRec ->
            %% we have a new record, go back go the topproplist
            to_rec({struct,Pl}, Module, SubRec)
    end,
    UpRec = module_set(Module, {Field,Value}, Rec),
    keys_rec(Rest, Module, Discriminators, UpRec);

keys_rec([{Key, Value}|Rest], Module, Discriminators, Rec) ->
    Field = binary_to_atom(Key),
    NewValue = to_value(Value,Module,Discriminators),
    NewRec = module_set(Module, {Field, NewValue}, Rec),
    keys_rec(Rest,Module,Discriminators,NewRec).

pl(P, Module, Discriminators) ->
    pl(P,Module,Discriminators,[]).
pl([],_M,_D,[H]) -> H;
pl([],_M,_D,Acc) -> lists:reverse(Acc);
pl([{_Key, {struct,_Pl}=Val}|Rest], Module, Discriminators, Acc) when length(Discriminators) > 0 ->
    pl(Rest, Module, Discriminators, [to_rec(Val,Module,Discriminators)|Acc]);
pl([{Key, {struct,Pl}}|Rest], Module, D, Acc) ->
    Value = case module_new(Module,Key,undefined) of
                undefined ->
                    {Key, pl(Pl, Module, [])};
                Rec ->
                    to_rec({struct, Pl}, Module, Rec)
            end,
    pl(Rest, Module, D, [Value|Acc]);
pl([{Key,Value}|Rest], Module, D, Acc) ->
    pl(Rest, Module, D, [{Key,Value}|Acc]).

to_value(V, Module, Discriminators) ->
    to_value(V, Module, Discriminators, []).

to_value({struct, Pl}, Module, Discriminators, _Acc) ->
    to_rec({struct, Pl},Module, Discriminators);
to_value([], _Module, _Discriminators, Acc) -> Acc;
to_value([H|T],Module,Discriminators, Acc) ->
    to_value(T,Module,Discriminators,[to_value(H,Module,Discriminators,[])|Acc]);
to_value(V,_Module,_Discriminators,_Acc) -> V.



module_new([], _Key, Rec) ->
    Rec;
module_new([H|T], Key, Rec) ->
    case H:new(Key) of
        undefined ->
            module_new(T,Key,Rec);
        SubRec ->
            SubRec
    end.


module_has_rec(Ms, Rec) ->
    module_has_rec(Ms, Rec, throw).

module_has_rec([],_Rec, throw) -> throw(did_not_find_module);
module_has_rec([],_Rec, V) -> V;
module_has_rec([M|T],Rec, Act) ->
    case M:'#is_record-'(Rec) of
        false ->
            module_has_rec(T,Rec, Act);
        true  ->
            M
    end.



module_set(Ms, Kv, Rec) ->
    M = module_has_rec(Ms,Rec),
    M:'#set-'([Kv],Rec).

module_rec_fields(Ms, Rec ) ->
    M = module_has_rec(Ms,Rec),
    M:'#info-'(element(1, Rec)).

module_get(Ms, Field, Rec) ->
    M = module_has_rec(Ms, Rec),
    M:'#get-'(Field,Rec).

has_discriminator(_Pl, _Module, []) -> false;
has_discriminator(Pl, Module, [D|Discriminators]) ->
    case lists:keytake(D, 1, Pl) of
        false -> has_discriminator(Pl, Module, Discriminators);
        {value, {D, Type}, Pl2} -> {Type, Pl2}
    end
    .

atom_to_binary(A) -> list_to_binary(atom_to_list(A)).
binary_to_atom(B) -> list_to_atom(binary_to_list(B)).

