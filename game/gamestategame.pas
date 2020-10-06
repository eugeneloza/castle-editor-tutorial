unit GameStateGame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  CastleUiState, CastleControls, CastleUiControls, CastleKeysMouse;

type
  TGamePad = record
    Group: TCastleUserInterface;
    Image: TCastleImageControl;
    Caption: TCastleLabel;
  end;

type
  TStateGame = class(TUiState)
  private
    GamePads: array[1..3, 1..4] of TGamePad;
    ScoreText, ScoreLabel, HighScoreText, HighScoreLabel: TCastleLabel;
    procedure ButtonPress(const Sender: TInputListener; const Event: TInputPressRelease; var Handled: Boolean);
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
  X, Y: Integer;
begin
  inherited;
  InsertUserInterface('castle-data:/Game.castle-user-interface', FreeAtStop, UiOwner);
  ScoreText := UiOwner.FindRequiredComponent('ScoreText') as TCastleLabel;
  ScoreLabel := UiOwner.FindRequiredComponent('ScoreLabel') as TCastleLabel;
  HighScoreText := UiOwner.FindRequiredComponent('HighScoreText') as TCastleLabel;
  HighScoreLabel := UiOwner.FindRequiredComponent('HighScoreLabel') as TCastleLabel;
  ScoreText.CustomFont := CartoonFont60;
  HighscoreText.CustomFont := CartoonFont30;
  for X := 1 to 3 do
    for Y := 1 to 4 do
    begin
      GamePads[X, Y].Group := UiOwner.FindRequiredComponent('ButtonGroup' + Y.ToString + X.ToString) as TCastleUserInterface;
      GamePads[X, Y].Group.OnPress := @ButtonPress;
      GamePads[X, Y].Image := UiOwner.FindRequiredComponent('Button' + Y.ToString + X.ToString) as TCastleImageControl;
      GamePads[X, Y].Caption := UiOwner.FindRequiredComponent('Label' + Y.ToString + X.ToString) as TCastleLabel;
    end;
end;

procedure TStateGame.ButtonPress(const Sender: TInputListener; const Event: TInputPressRelease; var Handled: Boolean);
var
  ThisGamePad: ^TGamePad;
begin
  if Event.EventType = itMouseButton then
  begin
    case Sender.Name of
      'ButtonGroup11': ThisGamePad := @GamePads[1, 1];
      'ButtonGroup12': ThisGamePad := @GamePads[2, 1];
      'ButtonGroup13': ThisGamePad := @GamePads[3, 1];
      'ButtonGroup21': ThisGamePad := @GamePads[1, 2];
      'ButtonGroup22': ThisGamePad := @GamePads[2, 2];
      'ButtonGroup23': ThisGamePad := @GamePads[3, 2];
      'ButtonGroup31': ThisGamePad := @GamePads[1, 3];
      'ButtonGroup32': ThisGamePad := @GamePads[2, 3];
      'ButtonGroup33': ThisGamePad := @GamePads[3, 3];
      'ButtonGroup41': ThisGamePad := @GamePads[1, 4];
      'ButtonGroup42': ThisGamePad := @GamePads[2, 4];
      'ButtonGroup43': ThisGamePad := @GamePads[3, 4];
      else
        raise Exception.Create('Unexpected Button name: ' + Sender.Name);
    end;
    ThisGamePad^.Caption.Caption := '!!!';
  end;
end;

end.

