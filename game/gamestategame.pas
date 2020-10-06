unit GameStateGame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  CastleUiState, CastleControls, CastleUiControls, CastleKeysMouse;

type
  TQuickButton = class(TCastleUserInterface)
  public
    OnPress: TNotifyEvent;
    function Press(const Event: TInputPressRelease): Boolean;
  end;

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

function TQuickButton.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Event.EventType = itMouseButton then
    OnPress;
end;

end.

