unit GameStateGameOver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  CastleUiState, CastleControls, CastleUiControls;

type
  TStateGameOver = class(TUiState)
  private
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
end;

end.

