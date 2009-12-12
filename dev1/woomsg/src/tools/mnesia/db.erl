-module(db).

-export([set/3, dec/3, inc/3]).    %% set/increase/decrease field value
-export([get/3]).                  %% retrive field value
-export([find/1, find/2, find/3]). %% retrive rows
-export([delete/1, delete/2]).     %% delete rows

%% External APIs:

%% Update Field in Table with Value
%% using Key to lookup the record.
%% Fun is fun(OldFieldValue, Value)
%% and should return the new value 
%% of the field or the tupe {error, reason}.
set(Table, Key, {Field, Value}, Fun) 
  when is_atom(Table),
       is_atom(Field) ->
    Fields = mnesia:table_info(Table, attributes),
    case fieldnum(Field, Fields) of
	none ->
	    {atomic, {error, field_not_found}};
	N ->
	    F = fun() ->
			case mnesia:read({Table, Key}) of
			    [] ->
				{error, key_not_found};
			    [Data] ->
				case Fun(element(N + 1, Data), Value) of
				    {error, Reason} ->
					{error, Reason};
				    Value1 ->
					Data1 = setelement(N + 1, 
							   Data, 
							   Value1),
					mnesia:write(Data1)
				end;
			    Any ->
				Any
			end
		end,
	    mnesia:transaction(F)
    end.

set(Table, Key, {Field, _Value} = V) 
  when is_atom(Table),
       is_atom(Field) ->
    F = fun(_Old, New) -> New end,
    set(Table, Key, V, F);

%% Simple set using a list of fields and values

set(Table, Key, Values) 
  when is_atom(Table),
       is_list(Values) ->
    Fields = mnesia:table_info(Table, attributes),
    case find(Table, Key) of
	{atomic, [Data]} ->
	    set(Data, Fields, Values);
	Any ->
	    Any
    end;

set(Data, _Fields, []) ->
    mnesia:transaction(fun() ->
			       mnesia:write(Data)
		       end);

set(Data, Fields, [{Field, Value}|Rest]) 
  when is_tuple(Data),
       is_list(Fields),
       is_atom(Field) ->
    case fieldnum(Field, Fields) of
	none ->
	    {atomic, {error, field_not_found}};
	N ->
	    Data1 = setelement(N + 1, 
			       Data, 
			       Value),
	    set(Data1, Fields, Rest)
    end.

dec(Table, Key, Field) 
  when is_atom(Table),
       is_tuple(Field) ->
    F = fun(Balance, Amount) ->
		if 
		    Amount > Balance ->
			{error, out_of_balance};
		    true ->
			Balance - Amount
		end
	end,
    set(Table, Key, Field, F).

inc(Table, Key, Field) 
  when is_atom(Table),
       is_tuple(Field) ->
    F = fun(Balance, Amount) -> Balance + Amount end,
    set(Table, Key, Field, F).



%% Retrieve value in Table 
%% using Key to lookup the record.
get(Table, Key, Field)
  when is_atom(Table),
       is_atom(Field) ->
    Fields = mnesia:table_info(Table, attributes),
    case fieldnum(Field, Fields) of
	none ->
	    {atomic, {error, field_not_found}};
	N ->
	    F = fun() ->
			case mnesia:read({Table, Key}) of
			    [] ->
				{error, key_not_found};
			    [Data] ->
				element(N + 1, Data);
			    Any ->
				Any
			end
		end,
	    mnesia:transaction(F)
    end.

%% retrieve all table rows
find(Table) 
  when is_atom(Table) ->
    Pat = makepat(Table),
    F = fun() -> mnesia:match_object(Pat) end,
    mnesia:transaction(F).

%% Lookup using primary key value
find(Table, Key) 
  when is_atom(Table) ->
    F = fun() -> mnesia:read({Table, Key}) end,
    mnesia:transaction(F).
    
%% Lookup using a secondary index
find(Table, Field, Value) 
  when is_atom(Table),
       is_atom(Field) ->
    Fields = mnesia:table_info(Table, attributes),
    case fieldnum(Field, Fields) of
	none ->
	    {atomic, {error, field_not_found}};
	N ->
	    F = fun() -> 
			mnesia:index_read(Table, Value, N + 1) 
		end,
	    mnesia:transaction(F)
    end.

delete(Table) 
  when is_atom(Table) ->
    mnesia:clear_table(Table).

delete(Table, Key) 
  when is_atom(Table) ->
    F = fun() -> mnesia:delete({Table, Key}) end,
    mnesia:transaction(F).


%% Internal APIs:

%% Find the position of an atom in a list
%% 
%% Return
%%   Position:ingeter(), start from 1 to ....
%%   none
%%
%% E.g.
%% fieldnum(id, [id, username, age]) will return 1.
fieldnum(Field, []) 
  when is_atom(Field) ->
    none;

fieldnum(Field, Fields)
  when is_atom(Field),
       is_list(Fields) ->
    fieldnum(Field, Fields, 1).

fieldnum(_Field, [], _N) ->
    none;

fieldnum(Field, [H|T], N) ->
    if
	Field == H ->
	    N;
	true ->
	    fieldnum(Field, T, N + 1)
    end.

%% Make a {table_name, '_', '_', ...} pattern
%% to match and retrieve all table rows.
makepat(Table)
  when is_atom(Table) ->
    Fields = mnesia:table_info(Table, attributes),
    makepat(Fields, [Table]).

makepat([], Acc) ->
    list_to_tuple(lists:reverse(Acc));

makepat([_H|T], Acc) ->
    makepat(T, ['_'|Acc]).
