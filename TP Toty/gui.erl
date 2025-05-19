  -module(gui).

    -export([start/1, init/1]).

    -include_lib("wx/include/wx.hrl").

    start(Name) ->
        spawn(gui, init, [Name]).

    init(Name) ->
        Width = 200,
        Height = 200,
        Server = wx:new(), %Server will be the parent for the Frame
        Frame = wxFrame:new(Server, -1, Name, [{size,{Width, Height}}]),
        wxFrame:setBackgroundColour(Frame, {0, 0, 0}),
        wxFrame:show(Frame),
        loop(Frame,120,120,120).

    loop(Frame,R,G,B)->
        receive
            
            increment ->
                wxFrame:setBackgroundColour(Frame, {G, B,R+20}),
                wxFrame:refresh(Frame),
                loop(Frame,G,B,R+20);
           
            stop ->
                ok;
            Error ->
                io:format("gui: strange message ~w ~n", [Error]),
                loop(Frame,R,G,B)
        end.
