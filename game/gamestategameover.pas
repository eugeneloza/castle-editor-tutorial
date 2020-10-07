unit GameStateGameOver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  CastleUiState, CastleControls, CastleUiControls;

const
  AnimationDuration = 0.3;

type
  TStateGameOver = class(TUiState)
  private
    AnimationTime: Single;
    BackgroundColor: TCastleRectangleControl;
    GameOverPopup: TCastleUserInterface;
    GameOverImage, HighScoreImage: TCastleImageControl;
    ScoreTextLabel, ScoreValueLabel: TCastleLabel;
    PlayAgainButton, MainMenuButton: TCastleButton;
    procedure ClickPlayAgain(Sender: TObject);
    procedure ClickMainMenu(Sender: TObject);
  public
    Score: Integer;
    HighScore: Boolean;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  StateGameOver: TStateGameOver;

implementation
uses
  CastleComponentSerialize,
  CastleVectors,
  GameFont, GameStateGame, GameStateMainMenu;

procedure TStateGameOver.Start;
var
  UiOwner: TComponent;
  UnusedBooleanVariable: Boolean = false;
begin
  inherited;
  InsertUserInterface('castle-data:/GameOver.castle-user-interface', FreeAtStop, UiOwner);
  BackgroundColor := UiOwner.FindRequiredComponent('BackgroundColor') as TCastleRectangleControl;
  GameOverPopup := UiOwner.FindRequiredComponent('GameOverPopup') as TCastleUserInterface;
  GameOverImage := UiOwner.FindRequiredComponent('GameOverImage') as TCastleImageControl;
  HighScoreImage := UiOwner.FindRequiredComponent('HighScoreImage') as TCastleImageControl;
  ScoreTextLabel := UiOwner.FindRequiredComponent('ScoreTextLabel') as TCastleLabel;
  ScoreValueLabel := UiOwner.FindRequiredComponent('ScoreValueLabel') as TCastleLabel;
  PlayAgainButton := UiOwner.FindRequiredComponent('PlayAgainButton') as TCastleButton;
  MainMenuButton := UiOwner.FindRequiredComponent('MainMenuButton') as TCastleButton;
  ScoreTextLabel.CustomFont := CartoonFont60;
  PlayAgainButton.CustomFont := CartoonFont60;
  MainMenuButton.CustomFont := CartoonFont60;
  PlayAgainButton.OnClick := @ClickPlayAgain;
  MainMenuButton.OnClick := @ClickMainMenu;
  ScoreValueLabel.Caption := Score.ToString;
  HighScoreImage.Exists := HighScore;
  GameOverImage.Exists := not HighScore;
  Update(0, UnusedBooleanVariable);
end;

procedure TStateGameOver.ClickPlayAgain(Sender: TObject);
begin
  TUiState.Current := StateGame;
end;

procedure TStateGameOver.ClickMainMenu(Sender: TObject);
begin
  TUiState.Current := StateMainMenu;
end;

procedure TStateGameOver.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  AnimationTime += SecondsPassed;
  if AnimationTime < AnimationDuration then
  begin
    BackgroundColor.Color := Vector4(0.57, 0.80, 0.92, 0.9 * AnimationTime/AnimationDuration);
    GameOverPopup.VerticalAnchorDelta := -0.5 * 1334 * (1.0 - Sqrt(AnimationTime/AnimationDuration));
  end else
  begin
    BackgroundColor.Color := Vector4(0.57, 0.80, 0.92, 0.9);
    GameOverPopup.VerticalAnchorDelta := 0;
  end;
end;

end.

