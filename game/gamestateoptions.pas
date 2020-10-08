unit GameStateOptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, CastleUiState, CastleControls;

type
  TStateOptions = class(TUiState)
  private
    ButtonVolume0, ButtonVolume1, ButtonVolume2, ButtonVolume3, ButtonVolume4,
      ButtonMusic0, ButtonMusic1, ButtonMusic2, ButtonMusic3, ButtonMusic4,
      ButtonVibration0, ButtonVibration1, BackButton: TCastleButton;
    SelectedVolume0, SelectedVolume1, SelectedVolume2, SelectedVolume3,
      SelectedVolume4, SelectedMusic0, SelectedMusic1, SelectedMusic2,
      SelectedMusic3, SelectedMusic4, SelectedVibration0, SelectedVibration1:
      TCastleImageControl;
    VibrationGroup: TCastleHorizontalGroup;
    procedure UpdateButtonsSelection;
    procedure SetVolume(const Volume: Single);
    procedure SetMusic(const Music: Single);
    procedure ClickVolume0(Sender: TObject);
    procedure ClickVolume1(Sender: TObject);
    procedure ClickVolume2(Sender: TObject);
    procedure ClickVolume3(Sender: TObject);
    procedure ClickVolume4(Sender: TObject);
    procedure ClickMusic0(Sender: TObject);
    procedure ClickMusic1(Sender: TObject);
    procedure ClickMusic2(Sender: TObject);
    procedure ClickMusic3(Sender: TObject);
    procedure ClickMusic4(Sender: TObject);
    procedure ClickVibrationOff(Sender: TObject);
    procedure ClickVibrationOn(Sender: TObject);
    procedure ClickBack(Sender: TObject);
  public
    procedure Start; override;
  end;

var
  StateOptions: TStateOptions;

implementation
uses
  CastleComponentSerialize,
  CastleSoundEngine, CastleConfig,
  GameFont, GameStateMainMenu;

procedure TStateOptions.Start;
var
  UiOwner: TComponent;
begin
  inherited;
  InsertUserInterface('castle-data:/Options.castle-user-interface', FreeAtStop, UiOwner);
  ButtonVolume0 := UiOwner.FindRequiredComponent('ButtonVolume0') as TCastleButton;
  ButtonVolume1 := UiOwner.FindRequiredComponent('ButtonVolume1') as TCastleButton;
  ButtonVolume2 := UiOwner.FindRequiredComponent('ButtonVolume2') as TCastleButton;
  ButtonVolume3 := UiOwner.FindRequiredComponent('ButtonVolume3') as TCastleButton;
  ButtonVolume4 := UiOwner.FindRequiredComponent('ButtonVolume4') as TCastleButton;
  ButtonMusic0 := UiOwner.FindRequiredComponent('ButtonMusic0') as TCastleButton;
  ButtonMusic1 := UiOwner.FindRequiredComponent('ButtonMusic1') as TCastleButton;
  ButtonMusic2 := UiOwner.FindRequiredComponent('ButtonMusic2') as TCastleButton;
  ButtonMusic3 := UiOwner.FindRequiredComponent('ButtonMusic3') as TCastleButton;
  ButtonMusic4 := UiOwner.FindRequiredComponent('ButtonMusic4') as TCastleButton;
  ButtonVibration0 := UiOwner.FindRequiredComponent('ButtonVibration0') as TCastleButton;
  ButtonVibration1 := UiOwner.FindRequiredComponent('ButtonVibration1') as TCastleButton;
  BackButton := UiOwner.FindRequiredComponent('BackButton') as TCastleButton;
  SelectedVolume0 := UiOwner.FindRequiredComponent('SelectedVolume0') as TCastleImageControl;
  SelectedVolume1 := UiOwner.FindRequiredComponent('SelectedVolume1') as TCastleImageControl;
  SelectedVolume2 := UiOwner.FindRequiredComponent('SelectedVolume2') as TCastleImageControl;
  SelectedVolume3 := UiOwner.FindRequiredComponent('SelectedVolume3') as TCastleImageControl;
  SelectedVolume4 := UiOwner.FindRequiredComponent('SelectedVolume4') as TCastleImageControl;
  SelectedMusic0 := UiOwner.FindRequiredComponent('SelectedMusic0') as TCastleImageControl;
  SelectedMusic1 := UiOwner.FindRequiredComponent('SelectedMusic1') as TCastleImageControl;
  SelectedMusic2 := UiOwner.FindRequiredComponent('SelectedMusic2') as TCastleImageControl;
  SelectedMusic3 := UiOwner.FindRequiredComponent('SelectedMusic3') as TCastleImageControl;
  SelectedMusic4 := UiOwner.FindRequiredComponent('SelectedMusic4') as TCastleImageControl;
  SelectedVibration0 := UiOwner.FindRequiredComponent('SelectedVibration0') as TCastleImageControl;
  SelectedVibration1 := UiOwner.FindRequiredComponent('SelectedVibration1') as TCastleImageControl;

  ButtonVolume0.OnClick := @ClickVolume0;
  ButtonVolume1.OnClick := @ClickVolume1;
  ButtonVolume2.OnClick := @ClickVolume2;
  ButtonVolume3.OnClick := @ClickVolume3;
  ButtonVolume4.OnClick := @ClickVolume4;
  ButtonMusic0.OnClick := @ClickMusic0;
  ButtonMusic1.OnClick := @ClickMusic1;
  ButtonMusic2.OnClick := @ClickMusic2;
  ButtonMusic3.OnClick := @ClickMusic3;
  ButtonMusic4.OnClick := @ClickMusic4;
  ButtonVibration0.OnClick := @ClickVibrationOff;
  ButtonVibration1.OnClick := @ClickVibrationOn;

  BackButton.OnClick := @ClickBack;
  BackButton.CustomFont := CartoonFont60;

  VibrationGroup := UiOwner.FindRequiredComponent('VibrationGroup') as TCastleHorizontalGroup;
  {$ifndef CASTLE_IOS}
    {$ifndef ANDROID}
      VibrationGroup.Exists := false;
    {$endif}
  {$endif}

  UpdateButtonsSelection;
end;

procedure TStateOptions.UpdateButtonsSelection;
var
  VolumeLevel, MusicLevel: Integer;
begin
  VolumeLevel := Round(UserConfig.GetFloat('volume', 1.0) * 4);
  SelectedVolume0.Exists := VolumeLevel = 0;
  SelectedVolume1.Exists := VolumeLevel = 1;
  SelectedVolume2.Exists := VolumeLevel = 2;
  SelectedVolume3.Exists := VolumeLevel = 3;
  SelectedVolume4.Exists := VolumeLevel = 4;
  MusicLevel := Round(UserConfig.GetFloat('music', 0.5) * 4);
  SelectedMusic0.Exists := MusicLevel = 0;
  SelectedMusic1.Exists := MusicLevel = 1;
  SelectedMusic2.Exists := MusicLevel = 2;
  SelectedMusic3.Exists := MusicLevel = 3;
  SelectedMusic4.Exists := MusicLevel = 4;
  SelectedVibration0.Exists := not UserConfig.GetValue('vibration', true);
  SelectedVibration1.Exists := UserConfig.GetValue('vibration', true);
end;

procedure TStateOptions.SetVolume(const Volume: Single);
begin
  SoundEngine.Volume := Volume;
  UserConfig.SetFloat('volume', Volume);
  UserConfig.Save;
  UpdateButtonsSelection;
end;

procedure TStateOptions.SetMusic(const Music: Single);
begin
  SoundEngine.LoopingChannel[0].Volume := Music;
  UserConfig.SetFloat('music', Music);
  UserConfig.Save;
  UpdateButtonsSelection;
end;

procedure TStateOptions.ClickVolume0(Sender: TObject);
begin
  SetVolume(0.0);
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
end;

procedure TStateOptions.ClickVolume1(Sender: TObject);
begin
  SetVolume(0.25);
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
end;

procedure TStateOptions.ClickVolume2(Sender: TObject);
begin
  SetVolume(0.5);
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
end;

procedure TStateOptions.ClickVolume3(Sender: TObject);
begin
  SetVolume(0.75);
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
end;

procedure TStateOptions.ClickVolume4(Sender: TObject);
begin
  SetVolume(1.0);
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
end;

procedure TStateOptions.ClickMusic0(Sender: TObject);
begin
  SetMusic(0.0);
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
end;

procedure TStateOptions.ClickMusic1(Sender: TObject);
begin
  SetMusic(0.25);
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
end;

procedure TStateOptions.ClickMusic2(Sender: TObject);
begin
  SetMusic(0.5);
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
end;

procedure TStateOptions.ClickMusic3(Sender: TObject);
begin
  SetMusic(0.75);
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
end;

procedure TStateOptions.ClickMusic4(Sender: TObject);
begin
  SetMusic(1.0);
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
end;

procedure TStateOptions.ClickVibrationOff(Sender: TObject);
begin
  UserConfig.SetValue('vibration', false);
  UserConfig.Save;
  UpdateButtonsSelection;
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
end;

procedure TStateOptions.ClickVibrationOn(Sender: TObject);
begin
  UserConfig.SetValue('vibration', true);
  UserConfig.Save;
  UpdateButtonsSelection;
  SoundEngine.Sound(SoundEngine.SoundFromName('ui_click'));
end;

procedure TStateOptions.ClickBack(Sender: TObject);
begin
  TUiState.Current := StateMainMenu;
  SoundEngine.Sound(SoundEngine.SoundFromName('quit'));
end;

end.

