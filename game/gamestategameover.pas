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
    DesignAchievement: TCastleDesign;
    procedure ClickPlayAgain(Sender: TObject);
    procedure ClickMainMenu(Sender: TObject);
  public
    Score: Integer;
    HighScore: Boolean;
    Achievement: String;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  StateGameOver: TStateGameOver;

implementation
uses
  CastleComponentSerialize,
  CastleVectors, CastleSoundEngine,
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
  DesignAchievement := UiOwner.FindRequiredComponent('DesignAchievement') as TCastleDesign;
  case Achievement of
    'achievement1': DesignAchievement.Url := 'castle-data:/achievement1.castle-user-interface';
    'achievement2': DesignAchievement.Url := 'castle-data:/achievement2.castle-user-interface';
    'achievement3': DesignAchievement.Url := 'castle-data:/achievement3.castle-user-interface';
    'achievement4': DesignAchievement.Url := 'castle-data:/achievement4.castle-user-interface';
    'achievement5': DesignAchievement.Url := 'castle-data:/achievement5.castle-user-interface';
    else DesignAchievement.Exists := false;
  end;
  ScoreTextLabel.CustomFont := CartoonFont60;
  PlayAgainButton.CustomFont := CartoonFont60;
  MainMenuButton.CustomFont := CartoonFont60;
  PlayAgainButton.OnClick := @ClickPlayAgain;
  MainMenuButton.OnClick := @ClickMainMenu;
  ScoreValueLabel.Caption := Score.ToString;
  HighScoreImage.Exists := HighScore;
  GameOverImage.Exists := not HighScore;
  AnimationTime := 0;
  Update(0, UnusedBooleanVariable);
end;

procedure TStateGameOver.ClickPlayAgain(Sender: TObject);
begin
  SoundEngine.Sound(SoundEngine.SoundFromName('start_game'));
  TUiState.Current := StateGame;
end;

procedure TStateGameOver.ClickMainMenu(Sender: TObject);
begin
  SoundEngine.Sound(SoundEngine.SoundFromName('quit'));
  TUiState.Current := StateMainMenu;
end;

procedure TStateGameOver.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  AnimationTime += SecondsPassed;
  if AnimationTime < AnimationDuration then
  begin
    BackgroundColor.Color := Vector4(0.57, 0.80, 0.92, 0.9 * AnimationTime/AnimationDuration);
    GameOverPopup.VerticalAnchorDelta := -0.5 * 1334 * (1.0 - AnimationTime/AnimationDuration);
  end else
  begin
    BackgroundColor.Color := Vector4(0.57, 0.80, 0.92, 0.9);
    GameOverPopup.VerticalAnchorDelta := 0;
  end;
end;

end.

