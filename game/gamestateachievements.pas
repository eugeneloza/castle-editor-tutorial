unit GameStateAchievements;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CastleUiState, CastleControls, CastleKeysMouse, CastleImages;

type
  TStateAchievements = class(TUiState)
  private
    NoAchievementImage: TCastleImage;
  public
    procedure Start; override;
    function Press(const Event: TInputPressRelease): Boolean; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

var
  StateAchievements: TStateAchievements;

implementation
uses
  CastleComponentSerialize,
  CastleSoundEngine, CastleConfig,
  GameStateMainMenu, GameFont;

constructor TStateAchievements.Create(AOwner: TComponent);
begin
  inherited;
  NoAchievementImage := LoadImage('castle-data:/images/buttons/achievement_0.png', [TRGBAlphaImage]);
end;

destructor TStateAchievements.Destroy;
begin
  FreeAndNil(NoAchievementImage);
  inherited;
end;

procedure TStateAchievements.Start;
var
  UiOwner: TComponent;
  DesignAchievement1, DesignAchievement2, DesignAchievement3,
    DesignAchievement4, DesignAchievement5: TCastleDesign;
begin
  inherited;
  InsertUserInterface('castle-data:/Achievements.castle-user-interface', FreeAtStop, UiOwner);
  if not UserConfig.GetValue('achievement1', false) then
  begin
    DesignAchievement1 := UiOwner.FindRequiredComponent('DesignAchievement1') as TCastleDesign;
    (DesignAchievement1.FindRequiredComponent('AchievementImage') as TCastleImageControl).Image := NoAchievementImage;
    (DesignAchievement1.FindRequiredComponent('AchievementImage') as TCastleImageControl).OwnsImage := false;
    (DesignAchievement1.FindRequiredComponent('AchievementDescription') as TCastleLabel).Exists := false;
    (DesignAchievement1.FindRequiredComponent('AchievementCaption') as TCastleLabel).Exists := false;
  end;
  if not UserConfig.GetValue('achievement2', false) then
  begin
    DesignAchievement2 := UiOwner.FindRequiredComponent('DesignAchievement2') as TCastleDesign;
    (DesignAchievement2.FindRequiredComponent('AchievementImage') as TCastleImageControl).Image := NoAchievementImage;
    (DesignAchievement2.FindRequiredComponent('AchievementImage') as TCastleImageControl).OwnsImage := false;
    (DesignAchievement2.FindRequiredComponent('AchievementDescription') as TCastleLabel).Exists := false;
    (DesignAchievement2.FindRequiredComponent('AchievementCaption') as TCastleLabel).Exists := false;
  end;
  if not UserConfig.GetValue('achievement3', false) then
  begin
    DesignAchievement3 := UiOwner.FindRequiredComponent('DesignAchievement3') as TCastleDesign;
    (DesignAchievement3.FindRequiredComponent('AchievementImage') as TCastleImageControl).Image := NoAchievementImage;
    (DesignAchievement3.FindRequiredComponent('AchievementImage') as TCastleImageControl).OwnsImage := false;
    (DesignAchievement3.FindRequiredComponent('AchievementDescription') as TCastleLabel).Exists := false;
    (DesignAchievement3.FindRequiredComponent('AchievementCaption') as TCastleLabel).Exists := false;
  end;
  if not UserConfig.GetValue('achievement4', false) then
  begin
    DesignAchievement4 := UiOwner.FindRequiredComponent('DesignAchievement4') as TCastleDesign;
    (DesignAchievement4.FindRequiredComponent('AchievementImage') as TCastleImageControl).Image := NoAchievementImage;
    (DesignAchievement4.FindRequiredComponent('AchievementImage') as TCastleImageControl).OwnsImage := false;
    (DesignAchievement4.FindRequiredComponent('AchievementDescription') as TCastleLabel).Exists := false;
    (DesignAchievement4.FindRequiredComponent('AchievementCaption') as TCastleLabel).Exists := false;
  end;
  if not UserConfig.GetValue('achievement5', false) then
  begin
    DesignAchievement5 := UiOwner.FindRequiredComponent('DesignAchievement5') as TCastleDesign;
    (DesignAchievement5.FindRequiredComponent('AchievementImage') as TCastleImageControl).Image := NoAchievementImage;
    (DesignAchievement5.FindRequiredComponent('AchievementImage') as TCastleImageControl).OwnsImage := false;
    (DesignAchievement5.FindRequiredComponent('AchievementDescription') as TCastleLabel).Exists := false;
    (DesignAchievement5.FindRequiredComponent('AchievementCaption') as TCastleLabel).Exists := false;
  end;

end;

function TStateAchievements.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  SoundEngine.Sound(SoundEngine.SoundFromName('quit'));
  TUiState.Current := StateMainMenu;
end;

end.

