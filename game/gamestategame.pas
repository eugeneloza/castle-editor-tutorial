unit GameStateGame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  CastleUiState;

type
  TStateGame = class(TUiState)
  private
  public
    procedure Start; override;
  end;

var
  StateGame: TStateGame;

implementation

procedure TStateGame.Start;
begin
  inherited;
end;

end.

