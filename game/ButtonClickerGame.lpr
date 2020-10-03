{ Program to run the game on desktop (standalone) platforms.
  Generated by the "Castle Game Engine" build tool. }
program ButtonClickerGame;

{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

{ This adds icons and version info for Windows,
  automatically created by "castle-engine compile". }
{$ifdef CASTLE_AUTO_GENERATED_RESOURCES} {$R castle-auto-generated-resources.res} {$endif}

uses
  CastleApplicationProperties, CastleLog, castle_base, castle_window,
  CastleWindow, GameInitialize;

begin
  ApplicationProperties.Version := '0.1';
  Application.ParseStandardParameters;

  { On standalone, activate log only after parsing command-line options.
    This allows to handle --version and --help command-line parameters
    without any extra output on Unix. }
  InitializeLog;

  Application.MainWindow.OpenAndRun;
end.