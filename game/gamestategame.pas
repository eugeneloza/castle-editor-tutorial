unit GameStateGame;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  CastleUiState, CastleControls, CastleUiControls, CastleKeysMouse, CastleImages;

const
  GrowTime = 2;
  RipeTime = GrowTime + 10;

type
  TGamePad = record
    Speed: Single;
    Ripeness: Single;
    Score: Integer;
    Group: TCastleUserInterface;
    Image: TCastleImageControl;
    Caption: TCastleLabel;
  end;

type
  TStateGame = class(TUiState)
  private
    BrokenButton: TCastleImage;
    GamePace: Single;
    GameScore: Integer;
    GameRunning: Boolean;
    GamePads: array[1..3, 1..4] of TGamePad;
    ScoreText, ScoreLabel, HighScoreText, HighScoreLabel: TCastleLabel;
    procedure ButtonPress(const Sender: TInputListener; const Event: TInputPressRelease; var Handled: Boolean);
  public
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: boolean); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  StateGame: TStateGame;

implementation
uses
  CastleComponentSerialize,
  CastleVectors,
  GameFont;

constructor TStateGame.Create(AOwner: TComponent);
begin
  inherited;
  BrokenButton := LoadImage('castle-data:/images/buttons/button_broken.png', [TRGBAlphaImage]);
end;

destructor TStateGame.Destroy;
begin
  FreeAndNil(BrokenButton);
  inherited;
end;

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
  Randomize;
  for X := 1 to 3 do
    for Y := 1 to 4 do
    begin
      GamePads[X, Y].Group := UiOwner.FindRequiredComponent('ButtonGroup' + Y.ToString + X.ToString) as TCastleUserInterface;
      GamePads[X, Y].Group.OnPress := @ButtonPress;
      GamePads[X, Y].Image := UiOwner.FindRequiredComponent('Button' + Y.ToString + X.ToString) as TCastleImageControl;
      GamePads[X, Y].Caption := UiOwner.FindRequiredComponent('Label' + Y.ToString + X.ToString) as TCastleLabel;
      GamePads[X, Y].Score := 0;
      GamePads[X, Y].Ripeness := 0.0;
      GamePads[X, Y].Speed := 0.5 + Random;
    end;
  GamePace := 1.0;
  GameScore := 0;
  GameRunning := true;
end;

procedure TStateGame.ButtonPress(const Sender: TInputListener; const Event: TInputPressRelease; var Handled: Boolean);
var
  ThisGamePad: ^TGamePad;
begin
  if Event.EventType = itMouseButton then
  begin
    if GameRunning then
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
      if ThisGamePad^.Score > 0 then
      begin
        GameScore += ThisGamePad^.Score;
        ThisGamePad^.Score := 0;
        ThisGamePad^.Ripeness := 0.0;
        ThisGamePad^.Speed := 0.5 + Random;
      end else
      if ThisGamePad^.Score = 0 then
        GamePace += 0.5;
    end;
  end;
end;

procedure TStateGame.Update(const SecondsPassed: Single; var HandleInput: boolean);
var
  X, Y: Integer;
begin
  inherited;
  if GameRunning then
  begin
    for X := 1 to 3 do
      for Y := 1 to 4 do
      begin
        GamePads[X, Y].Ripeness += SecondsPassed * GamePace * GamePads[X, Y].Speed;
        if GamePads[X, Y].Ripeness < GrowTime then
        begin
          GamePads[X, Y].Score := 0;
          GamePads[X, Y].Caption.Exists := false;
          GamePads[X, Y].Image.Color := Vector4(GamePads[X, Y].Ripeness / GrowTime, 1.0, 0.0, 1.0);
        end else
        if GamePads[X, Y].Ripeness <= RipeTime then
        begin
          GamePads[X, Y].Score := 100 + Trunc(899 * (GamePads[X, Y].Ripeness - GrowTime) / (RipeTime - GrowTime));
          GamePads[X, Y].Caption.Exists := true;
          GamePads[X, Y].Caption.Caption := GamePads[X, Y].Score.ToString;
          GamePads[X, Y].Image.Color := Vector4(1.0, 1.0 - (GamePads[X, Y].Ripeness - GrowTime) / (RipeTime - GrowTime), 0.0, 1.0);
        end else
        begin
          //GameOver
          GameRunning := false;
          GamePads[X, Y].Score := -1;
          GamePads[X, Y].Caption.Exists := false;
          GamePads[X, Y].Image.Image := BrokenButton;
          GamePads[X, Y].Image.OwnsImage := false;
          GamePads[X, Y].Image.Color := Vector4(1.0, 0.0, 0.0, 1.0);
        end;
      end;
    GamePace += SecondsPassed / 60;
  end;
  ScoreLabel.Caption := GameScore.ToString;
end;

end.

