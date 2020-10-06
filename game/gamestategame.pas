unit GameStateGame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  CastleUiState, CastleControls, CastleUiControls;

type
  TGamePad = class
    Group: TCastleUserInterface;
    Image: TCastleImageControl;
    Caption: TCastleLabel;
  end;

type
  TStateGame = class(TUiState)
  private
    ScoreText, ScoreLabel, HighScoreText, HighScoreLabel: TCastleLabel;
  public
    procedure Start; override;
  end;

var
  StateGame: TStateGame;

implementation
uses
  CastleComponentSerialize,
  GameFont;

procedure TStateGame.Start;
var
  UiOwner: TComponent;
begin
  inherited;
  InsertUserInterface('castle-data:/Game.castle-user-interface', FreeAtStop, UiOwner);
  ScoreText := UiOwner.FindRequiredComponent('ScoreText') as TCastleLabel;
  ScoreLabel := UiOwner.FindRequiredComponent('ScoreLabel') as TCastleLabel;
  HighScoreText := UiOwner.FindRequiredComponent('HighScoreText') as TCastleLabel;
  HighScoreLabel := UiOwner.FindRequiredComponent('HighScoreLabel') as TCastleLabel;
  ScoreText.CustomFont := CartoonFont60;
  HighscoreText.CustomFont := CartoonFont30;
end;

end.

