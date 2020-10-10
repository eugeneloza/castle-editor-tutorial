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
  CastleSoundEngine,
  GameStateMainMenu;

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
begin
  inherited;
  InsertUserInterface('castle-data:/Achievements.castle-user-interface', FreeAtStop, UiOwner);
end;

function TStateAchievements.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  SoundEngine.Sound(SoundEngine.SoundFromName('quit'));
  TUiState.Current := StateMainMenu;
end;

end.

