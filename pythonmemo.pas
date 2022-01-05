unit PythonMemo;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, RichMemo, RegExpr, Graphics, Dialogs, Windows,
  LazUTF8, LazUTF16, ClipBrd, Crt, FileHandle;

const
  MM_NORMAL = 0;
  MM_L1 = 1;
  MM_L2 = 2;

type

  TRule = record
    RE: TregExpr;
    RegExp: string;
    Color: TColor;
    Style: TFontstyles;
  end;


  { TRichMemo }

  TRichMemo = class(RichMemo.TRichMemo)
  protected
    procedure KeyDown(var Key: word; Shift: TShiftState); override;
    procedure KeyUp(var Key: word; Shift: TShiftState); override;
    procedure DoEnter; override;
    procedure Click; override;


  private
  var
    Count: integer;
    FRules: array[1..80] of TRule;
    OldY: integer;
    function XYtoX(x, y: integer): integer;
    procedure TouchLine(n: integer);


  public
    constructor Create(AOwner: TComponent); override;
    procedure SaveTo(Filename: string);
    procedure Save();
    procedure OpenFrom(Filename: string);
    procedure Revert;
    function HasFile(): boolean;
    var CurrentFile : string;
  end;

implementation

function TRichMemo.HasFile(): boolean;
begin
  FileExists(CurrentFile);
end;

procedure TRichMemo.Save();
begin
SaveTextFile(CurrentFile, Lines);
end;

procedure TRichMemo.Revert;
begin
OpenFrom(CurrentFile);
end;

procedure TRichMemo.SaveTo(Filename: string);
begin
SaveTextFile(Filename, Lines);
CurrentFile := FileName;
end;

procedure TRichMemo.OpenFrom(Filename: string);
var i: integer;
begin
Lines.BeginUpdate;
Lines := OpenTextFile(FileName);
for i := 0 to Lines.Count - 1 do
    TouchLine(i);
Lines.EndUpdate;
CurrentFile := Filename;
end;

function TRichMemo.XYtoX(x, y: integer): integer;
begin
  XYtoX := SendMessage(Handle, EM_LINEINDEX, y, x);
end;

constructor TRichMemo.Create(AOwner: TComponent);

  procedure Add(s: string; c: TColor; Fs: TFontStyles);
  begin
    Count := Count + 1;
    FRules[Count].RE := TRegExpr.Create(s);
    FRules[Count].RegExp := s;
    FRules[Count].Color := c;
    FRules[Count].Style := Fs;
  end;

var
  i: integer;

begin
  inherited;
  OldY := 0;
  Count := 0;

  Add('\band\b|\bassert\b|\bbreak\b|\bclass\b|\bcontinue\b|\bdef\b|\bdel\b|\belif\b|\belse\b|\bexcept\b|\bexec\b|\bfinally\b|\bfor\b|\bfrom\b|\bglobal\b|\bif\b|\bimport\b|\bin\b|\bis\b|\blambda\b|\bnot\b|\bor\b|\bpass\b|\bprint\b|\braise\b|\breturn\b|\btry\b|\bwhile\b|\byield\b|\bNone\b|\bTrue\b|\bFalse', clBlack, [fsBold]);
  Add('=|<|>|\+|-|\*|/|\%|\^|\||\&|\~|\{|\}|\[|\]|\(|\)|\.|,|;|:|\\|!|@|&', clRed, []);
  Add('"[^"\\]*(\\.[^"\\]*)*"|''[^''\\]*(\\.[^''\\]*)*''|\b[+-]?[0-9]+[lL]?\b|\b[+-]?0[xX][0-9A-Fa-f]+[lL]?\b|\b[+-]?[0-9]+(?:\.[0-9]+)?(?:[eE][+-]?[0-9]+)?\b', clBlue, []);
  Add('#[^\n]*', clGreen, [fsItalic]);
  CurrentFile := '';
end;




procedure TRichMemo.TouchLine(n: integer);
var
  TF: TFont;
  off: integer;
  i: integer;
begin
  if Lines[n] = '' then Exit;
  off := XYtoX(0, n);
  TF := TFont.Create();
  TF.Name := Font.Name;
  TF.Size := Font.Size;
  TF.Color := clBlack;
  TF.Style := [];
  SetTextAttributes(off, Utf16Length(Lines[n]) + 1, TF);
  // Make everything plain black including the newlines (hence the +1).
  for i := 1 to Count do
  begin
    FRules[i].RE.InputString := self.Lines[n];
    if FRules[i].RE.Exec then
      repeat
        TF.Color := FRules[i].Color;
        TF.Style := FRules[i].Style;
        self.SetTextAttributes(off + Utf16Length(Copy(
          (Lines[n]), 1, FRules[i].RE.MatchPos[0] - 1)),
          Utf16Length(FRules[i].RE.Match[0]), TF)
      until not FRules[i].RE.ExecNext;
  end;
  TF.Free;
end;


function FirstNonSpace(s: string): integer;
begin
  // To be more accurate, it returns the number of leading spaces plus one. So the empty string returns 1.
  FirstNonSpace := 1;
  while (s <> '') and (s[FirstNonSpace] = ' ') and (FirstNonSpace <= Length(s)) do
    FirstNonSpace := FirstNonSpace + 1;
end;


function Whitespace(s: string): string;
var
  i: integer;
begin
  Whitespace := '';
  i := 1;
  while i <= Length(s) do
  begin
    if s[i] in [' ', #9] then
      Whitespace := Whitespace + s[i]
    else
      i := Length(s);
    i := i + 1;
  end;
end;


procedure TRichMemo.KeyUp(var Key: word; Shift: TShiftstate);
var
  i: integer;
  x, y: integer;
  Utf8x: integer;
  CurP, EndP: PChar;
  Len: integer;
  ACodePoint: string;
  q1, q2, Found: boolean;
  Quoted, temp: string;
  Qstart: integer;
  olen: integer;
  s, temps: string;
  OpenBracket, CloseBracket: string;
  BracketCount: integer;
  tempi: integer;
  TF: TFontParams;
begin

  x := self.CaretPos.x;
  y := self.CaretPos.y;


// We bracket-match.

OpenBracket := 'foo';
if (Key = $30) and (Shift = [ssShift]) then
begin
  OpenBracket := '(';
  CloseBracket := ')';
end;
if (Key = $DD) and (Shift = []) then
begin
  OpenBracket := '[';
  CloseBracket := ']';
end;
if (Key = $DD) and (Shift = [ssShift]) then
begin
  OpenBracket := '{';
  CloseBracket := '}';
end;
if OpenBracket <> 'foo' then
  begin
    BracketCount := 1;
    i := SelStart - 2;
    while (i >= 0) and (BracketCount > 0) do
    begin
      temps := GetText(i, 1);
      if temps = CloseBracket then BracketCount := BracketCount + 1;
      if temps = OpenBracket then BracketCount := BracketCount - 1;
      GetTextAttributes(i, TF);
      if TF.Style = [fsBold] then break;
      Dec(i);
    end;
    if BracketCount = 0 then
    begin
      SelText := '';
      Key := 0;
      TouchLine(y);
      tempi := SelStart;
      SelStart := i + 1;
      SelLength := tempi - SelStart;
      Delay(400);
      SelStart := tempi;
      Exit;
    end;
  end;

  //We handle the enter, tab, and backspace keys.

  if (Key = 13) and (y > 0) and (Lines[y - 1] <> '') then          // enter
  begin
      temps := Whitespace(Lines[y - 1]);
      if Lines[y - 1][Length(Lines[y - 1])] = ':' then temps := '    ' + temps;
      InDelText(temps, SelStart, 0);
      CaretPos := TPoint.Create(Length(temps), y);
  end;

  if Key = 9 then           // tab
  begin
      InDelText('    ', SelStart, 0);
      CaretPos := TPoint.Create(x + 4, y);
      Key := 0;
  end;


  // Then we do the syntax highlighting.

  if not (Key in [37, 40]) then
    // But there's no need to if the user's pressed an arrow key, because then nothing's changed.
  begin
    x := self.CaretPos.x;
    y := self.CaretPos.y;
    if y <= OldY then
      TouchLine(y)
    else
    begin
      for i := OldY to y do
        TouchLine(i);
    end;
    y := OldY;
  end;

  inherited;

end;

procedure TRichMemo.KeyDown(var Key: word; Shift: TShiftstate);
var
  y: integer;
  WS: WideString;
begin

  y := self.CaretPos.y;
  OldY := y;

  if (Key = Ord('V')) and (Shift = [ssCtrl]) or
    // This makes sure pasted text assumes the format of whatever it's pasted into.
    (Key = VK_INSERT) and (Shift = [ssShift]) then
  begin
    InDelText(Clipboard.AsText, SelStart, SelLength);
    SelStart := SelStart + Utf16Length(Clipboard.AsText);
    Key := 0;
  end;

  if (Key = Ord('Z')) and (Shift = [ssCtrl]) then
  begin
    WS := Text;
    Key := 0;
    Shift := [];
    while CanUndo do
    begin
      Undo;
      if WS <> Text then
      begin
        TouchLine(CaretPos.y);
        Break;
      end;
    end;
  end;

  inherited;

  if Key = 9 then Key := 0;

end;

procedure TRichMemo.Click;
begin
  OldY := CaretPos.y;
  inherited;
end;

procedure TRichMemo.DoEnter;
begin
  OldY := CaretPos.y;
  inherited;
end;


end.

