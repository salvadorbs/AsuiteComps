{$IFDEF FPC}
  {.$B-} // Boolean short-circuit evaluation ON
  {$H+} // Long strings ON
  {.$T-} // Type-checked pointers OFF
  {.$X+} // Extended syntax ON

  {$IFNDEF RO_FPC_MODE_SET}
    {$MODE Delphi}
  {$ENDIF}
  {.$DEFINE INDEBUG}
  
  {$IFDEF UNIX}
	  {$IFDEF LCLGTK2}
		{$DEFINE GTK}
	  {$ENDIF}
	  
	  {$IFDEF LCLGTK3}
		{$DEFINE GTK}
	  {$ENDIF}
	  
	  {$IFDEF LCLQT}
		{$DEFINE QT}
	  {$ENDIF}
	  
	  {$IFDEF LCLQT5}
		{$DEFINE QT}
	  {$ENDIF}
  {$ENDIF}
  
{$ELSE}
  {.$DEFINE INDEBUG}
{$ENDIF}
