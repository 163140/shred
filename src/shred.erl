-module(shred).
-include_lib("eunit/include/eunit.hrl").
-include_lib("kernel/include/file.hrl").
-export([coarse_check/1]).
-author("ea1a87").

-define(MAX_FILE_SIZE, 500000). % 500KB

%% Неиспользуемые в настоящий момент функции
-compile([{nowarn_unused_function, [ {coarse_check, 1} ] } ]).



-define(LINE_MAX_LENGTH, 10000). % 	на мой взгляд более 10 тыс символов в строке описания одной цепи не требуется
-define(MAX_SIZE, 500000). % bytes



%% @doc Грубая валидация вводного файла. Я не верю в схемы размером более 500кБ текста и более 500 элементов в цепи (т.е не более 10.000 символов в строке).

coarse_check(Filename) ->
	try
		Fileinfo = file:read_file_info(Filename, [{time, posix}]),
		OK = is_readable(Fileinfo) and is_small(Fileinfo) and
				 is_lines_short(Filename) and is_lines_structure_ok(Filename),
		OK
	catch
		throw:unreadable 		-> {error, "Не может быть прочитан"};
		throw:too_big		 		-> {error, "Более 500КБ"};
		throw:too_long_lines-> {error, "Более 10к символов в строке"};
		throw:not_correct		-> {error, "Некорректно заполненао описание"}
	end
.

%% Читаем ли файл.
is_readable(Fileinfo) ->
	{ok, _Info} = Fileinfo,
	Info = tuple_to_list(_Info),
	OK = lists:member(read, Info) or lists:member(read_write, Info),
	if not OK -> throw(unreadable);
		 true   -> true
  end.
%% Mенее ли он максимального размера
is_small(Fileinfo) ->
	{ok, {file_info, Size, _,_,_,_,_,_,_,_,_,_,_}} = Fileinfo,
	OK = Size < ?MAX_SIZE,
	if not OK -> throw(too_big);
		 true   -> true
  end.
%% Строки короче разумного значения?
is_lines_short(Filename) ->
	{ok, FD} = file:open(Filename, read),
	{ok, FirstLine} = file:read_line(FD),
	Lengths = read_n_length(FirstLine, [], FD),
	Maxlen	= lists:max(Lengths),
	file:close(FD),
	OK = Maxlen	< ?LINE_MAX_LENGTH,
	if not OK -> throw(too_long_lines);
		 true   -> true
  end.

read_n_length([], Lengths, _FD) -> Lengths;
read_n_length(Line, Lengths, FD) ->
	{ok, Nextline} = file:read_line(FD),
	L = length(Line),
	read_n_length(Nextline, [L|Lengths], FD).

% Поделены ли строки на элементарные элементы (хотя бы три).
is_lines_structure_ok(Filename) ->
	{ok, FD} = file:open(Filename, read),
	{ok, FirstLine} = file:read_line(FD),
	Status = is_single_line_ok(FirstLine, [], FD),
	Is_ok	 = lists:member(false, Status),
	Is_ok.

is_single_line_ok([], Status, _FD) -> Status;
is_single_line_ok(Line, Status, FD) ->
	{ok, Nextline} = file:read_line(FD),
	OK = is_structure_ok(Line),
	read_n_length(Nextline, [OK|Status], FD).

is_structure_ok(Line) ->
	Maybe_match = re:run(Line, ";", [global]),
	case Maybe_match of
		{match, Matches} -> OK = length(Matches) > 3;
		 nomatch         -> OK = false
	end,
	if not OK -> throw(not_correct);
		 true   -> true
  end.

%% @doc Тонкая валидация. Проверка файла на предмет коррекности введенных данных, например: марка кабеля, сечение и т.д.

scrutiny(Filename) ->
	Filecontent = read_to_lines(Filename),
	DATA = parse_data(Filecontent), % [{line1, [obj_name, obj_par1, ...]}, line2... ]
	OK   = check(DATA),
	stub,
	true.

read_to_lines(Filename) ->
	BIN_data = file:read("point.erl"),
	Lines		 = [binary_to_list(Line) || Line <- binary:split(
																							BIN_data, <<"\n">>,
																							[global]),
																			Line =/= << >>].

parse(Data) ->
stub.

parse_line(Line) ->
	DATA = parse_data(Filecontent), % [{line1, [obj_name, obj_par1, ...]}, line2... ]
	Data = binary:split(Line, <<";">>, [global]),
	pack().

structuring(List) ->
	Elements = (length(List) - 3)/6,

	

	









readlines(FileName) ->
    {ok, Data} = file:read_file(FileName),
		binary:split(Data, [<<"\n">>], [global]).
