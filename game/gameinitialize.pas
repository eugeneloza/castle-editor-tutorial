{ Game initialization.
  This unit is cross-platform.
  It will be used by the platform-specific program or library file.

  Feel free to use this code as a starting point for your own projects.
  (This code is in public domain, unlike most other CGE code which
  is covered by the LGPL license variant, see the COPYING.txt file.) }
unit GameInitialize;

interface

implementation

uses SysUtils,
  CastleWindow, CastleScene, CastleControls, CastleLog, CastleFilesUtils,
  CastleUIControls, CastleApplicationProperties, CastleUiState, CastleConfig,
  CastleSoundEngine,
  GameFont, GameStateMainMenu, GameStateGame, GameStateGameOver,
  GameStateOptions;

var
  Window: TCastleWindowBase;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  Window.Container.LoadSettings('castle-data:/CastleSettings.xml');

  LoadFonts;
  UserConfig.Load;
  SoundEngine.RepositoryURL := 'castle-data:/audio/index.xml';

  SoundEngine.Volume := UserConfig.GetFloat('volume', 1.0);
  SoundEngine.LoopingChannel[0].Volume := UserConfig.GetFloat('music', 0.5);

  StateGame := TStateGame.Create(Application);
  StateGameOver := TStateGameOver.Create(Application);
  StateOptions := TStateOptions.Create(Application);
  StateMainMenu := TStateMainMenu.Create(Application);
  TUiState.Current := StateMainMenu;
end;

initialization
  ApplicationProperties.ApplicationName := 'ButtonClickerGame';

  if IsLibrary then
    InitializeLog;

  { Initialize Application.OnInitialize. }
  Application.OnInitialize := @ApplicationInitialize;

  { Create and assign Application.MainWindow. }
  Window := TCastleWindowBase.Create(Application);
  Window.Caption := 'Button Clicker';
  Application.MainWindow := Window;
  {$ifndef CASTLE_IOS}
    {$ifndef ANDROID}
      Window.Height := Application.ScreenHeight * 5 div 6;
      Window.Width := Window.Height * 750 div 1334;
    {$endif}
  {$endif}
end.
