{$IFDEF CtrlsHnd}
  {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}

unit CtrlsHnd;

{{
}

interface

uses
    Classes, SysUtils, Controls, ComCtrls, StdCtrls, Types, Messages, Windows;


type
    TCtrlsHnd = class(TObject)
    public
        class function ForceFocus(AControl : TWinControl) : boolean;
        class function InvalidFocusControl(AControl : TWinControl) : TWinControl;
        class function GotoNextControl(AControl : TWinControl; FwdDir : boolean = True; Execute : boolean = True) : TWinControl;
    end;


    TRegURL = class(TStringList)
    {{
    Class for Used recent list ( URL ), yet not fully implemented. So need some work.
    }
    private
        FAllowBlanks : boolean;
        FCharCase :    TEditCharCase;
        FMaxSize :     Integer;
        FRegKey :      string;
        FUnique :      boolean;
        function CanAdd(Str : string) : boolean;
        function Equal(const Str1, Str2 : string) : boolean;
        procedure ReadItemsList(Strings : TStrings);
        procedure SetAllowBlanks(const Value : boolean);
        procedure SetCharCase(const Value : TEditCharCase);
        procedure SetUnique(const Value : boolean);
    protected
        procedure SetMaxSize(Value : Integer);
        procedure SetRegKey(Value : string);
    public
        constructor Create;
        destructor Destroy; override;
        function Add(const S : string) : Integer; override;
        procedure Commit;
        procedure Compact;
        function ReadFromINIFile(const IniFile, IniSection : string) : boolean;
        procedure Refresh;
        function SaveToINIFile(const IniFile, IniSection : PChar) : boolean;
        property AllowBlanks : boolean read FAllowBlanks write SetAllowBlanks;
        property CharCase : TEditCharCase read FCharCase write SetCharCase;
        property MaxSize : Integer read FMaxSize write SetMaxSize;
        property RegKey : string read FRegKey write SetRegKey;
        property Unique : boolean read FUnique write SetUnique;
    end;

procedure CenterControlInParent(AControl : TControl);
procedure ExchangePageControlSheets(PC : TPageControl; PI1, PI2 : Integer);
function MemoGetCaretPos(MemoObj : TMemo) : TPoint;
procedure MemoPrintTextPrn(MemoObj : TMemo);
procedure SetControlsVisibilty(Visibility : boolean; CList : array of TControl);
procedure WinControlsEnable(CList : array of TControl);
procedure WinControlsDisable(CList : array of TControl);

implementation

uses
    Forms, Math, WinReg32, Str_Pas, IniFiles, Printers, Contnrs;

type
    THackedWinControl = class(TWinControl)
    end;


procedure CenterControlInParent(AControl : TControl);
//----------------------------------------------------------------------------------------------------------------------
begin
    AControl.Top  := (AControl.Parent.Height - AControl.Height) div 2;
    AControl.Left := (AControl.Parent.Width - AControl.Width) div 2;
end;

procedure ExchangePageControlSheets(PC : TPageControl; PI1, PI2 : Integer);
//----------------------------------------------------------------------------------------------------------------------
var
    Q :     TList;
    Vl, i : Integer;
begin
    if (PI1 <= PC.PageCount) and (PI2 <= PC.PageCount) then begin
        if PI2 = PI1 then begin
            Exit;
        end;
        Vl := Min(PI1, PI2);
        Q  := TList.Create;
        try
            for i := PC.PageCount - 1 downto Vl do begin
                Q.Add(PC.Pages[i]);
                PC.Pages[i].PageControl := nil;
            end;
            Q.Exchange(Q.Count - 1, Q.Count - Abs(PI2 - PI1) - 1); //Troca posicoes
            for i := Q.Count - 1 downto 0 do begin
                TTabSheet(Q.Items[i]).PageControl := PC;
            end;
        finally
            Q.Free;
        end;
    end else begin
        raise EListError.CreateFmt('%s não possui %d páginas', [PC.Name, Max(PI1, PI2)]);
    end;
end;

function MemoGetCaretPos(MemoObj : TMemo) : TPoint;
    //----------------------------------------------------------------------------------------------------------------------------------
begin
    Result.Y := MemoObj.Perform(EM_LINEFROMCHAR, MemoObj.SelStart, 0);
    Result.X := MemoObj.SelStart - MemoObj.Perform(EM_LINEINDEX, Result.Y, 0);
end;

procedure MemoPrintTextPrn(MemoObj : TMemo);
//----------------------------------------------------------------------------------------------------------------------------------
var
    PrintFile : TextFile;
    I : Integer;
begin
    AssignPrn(PrintFile);      //atribui a impressora a um arquivo
    try
        Rewrite(PrintFile);
        Printer.Canvas.Font := MemoObj.Font;     //configura o fonte
        for I := 0 to MemoObj.Lines.Count - 1 do begin //copia todo o texto do memo para o arquivo de impressora
            Writeln(PrintFile, MemoObj.Lines[I]);
        end;
    finally
        System.CloseFile(PrintFile);
    end;
end;

procedure SetControlsVisibilty(Visibility : boolean; CList : array of TControl);
//----------------------------------------------------------------------------------------------------------------------
var
    i : Integer;
begin
    for i := Low(Clist) to High(CList) do begin
        CList[i].Visible := Visibility;
    end;
end;

procedure WinControlsEnable(CList : array of TControl);
//----------------------------------------------------------------------------------------------------------------------------------
var
    i : Integer;
begin
    for i := 0 to High(CList) do begin
        CList[i].Enabled := True;
    end;
end;

procedure WinControlsDisable(CList : array of TControl);
//----------------------------------------------------------------------------------------------------------------------------------
var
    i : Integer;
begin
    for i := 0 to High(CList) do begin
        CList[i].Enabled := False;
    end;
end;


{-**********************************************************************
************************************************************************
******************
******************  Class:    TRegURL
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
function TRegURL.CanAdd(Str : string) : boolean;
begin
    if (not AllowBlanks) and (Str = EmptyStr) then begin
        Result := False;
    end else begin //Testa a duplicidade
        if FUnique then begin
            case CharCase of
                ecNormal : begin
                    //Usa parametro passado
                end;
                ecUpperCase : begin
                    Str := UpperCase(Str);
                end;
                ecLowerCase : begin
                    Str := LowerCase(Str);
                end;
            end;
            Result := (Self.IndexOf(Str) = -1);
        end else begin
            Result := True;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegURL.Equal(const Str1, Str2 : string) : boolean;
begin
    if CharCase = ecNormal then begin
        Result := (StrIComp(PChar(Str1), PChar(Str2)) = 0);
    end else begin
        Result := (Str1 = Str2);
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegURL.ReadItemsList(Strings : TStrings);
var
    Reg :     TRegistryNT;
    i, Conv : Integer;
begin
    Reg := TRegistryNT.Create;
    try
        Reg.AccessMode := KEY_QUERY_VALUE;
        Reg.OpenFullKey(FRegKey, True);
        Reg.GetValueNames(Strings);
        if Strings.Count > 0 then begin
            Conv := -1;
            i    := 0;
            repeat
                try
                    Conv := StrToInt(Strings.Strings[i]);
                except
                    on Exception do begin
                        Conv := -1;
                    end;
                end;
                if Conv >= 0 then begin
                    Inc(i);
                end else begin
                    Strings.Delete(i);
                end;
            until (i >= Strings.Count);
        end;
    finally
        Reg.Free;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegURL.SetAllowBlanks(const Value : boolean);
begin
    FAllowBlanks := Value;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegURL.SetCharCase(const Value : TEditCharCase);
var
    i : Integer;

    //----------------------------------------------------------------------------------------------------------------------------------

begin
    if FCharCase <> Value then begin
        case Value of
            ecNormal : begin
                //Nada a fazer preservar o atual
            end;
            ecUpperCase : begin
                for i := 0 to Self.Count - 1 do begin
                    Self.Strings[i] := UpperCase(Self.Strings[i]);
                end;
            end;
            ecLowerCase : begin
                for i := 0 to Self.Count - 1 do begin
                    Self.Strings[i] := LowerCase(Self.Strings[i]);
                end;
            end;
        end;
        FCharCase := Value;
        Self.Commit;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegURL.SetUnique(const Value : boolean);

//----------------------------------------------------------------------------------------------------------------------------------

begin
    if FUnique xor Value then begin
        if Value then begin
            Str_Pas.RemoveDupStrList(Self, (FCharCase <> ecNormal));
        end;
        FUnique := Value;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegURL.SetMaxSize(Value : Integer);
var
    i : Integer;
begin
    if Value < Self.Count then begin
        for i := 0 to (Self.Count - Value - 1) do begin
            Self.Delete(1);
        end;
        Self.Commit; //Salva mudancas no registro
    end;
    Self.Capacity := Value;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegURL.SetRegKey(Value : string);
var
    Reg : TRegistryNT;
begin
    if not Equal(Value, FRegKey) then begin //Verifica mudanca de chave
        Reg := TRegistryNT.Create;
        try
            try
                //abre chave
                Reg.OpenFullKey(Value, True);
                FRegKey := Value;
                //Carrega dados para lista
                Self.Compact;
                Self.Refresh; //Recarrega lista
            except
                raise Exception.CreateFmt('Impossível acessar a chave %s do registro', [Value]);
            end;
        finally
            Reg.Free;
        end;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegURL.Add(const S : string) : Integer;
begin
    if Self.CanAdd(S) then begin
        Result := inherited Add(S);
    end else begin
        Result := -1;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegURL.Commit;
var
    Reg : TRegistryNT;
    i :   Integer;
begin
    Reg := TRegistryNT.Create;
    try
        Reg.AccessMode := KEY_WRITE;
        if Reg.OpenFullKey(Self.FRegKey, True) then begin
            Reg.ClearValues; //Limpa todas as entradas na chave
            for i := 0 to Self.Count - 1 do begin
                Reg.WriteString(IntToStr(i + 1), Self.Strings[i]);
            end;
        end else begin
            raise Exception.Create('Erro ao acessar: ' + Self.FRegKey);
        end;
    finally
        Reg.Free;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegURL.Compact;
var
    Reg :   TRegistryNT;
    List :  TStringList;
    Conv, i, Index : Integer;
    Value : string;
begin
    Reg  := TRegistryNT.Create;
    List := TStringList.Create;
    try
        Reg.AccessMode := KEY_WRITE;
        Reg.OpenFullKey(FRegKey, True);
        Reg.GetValueNames(List);
        Index := 1;
        Conv  := -1;
        for i := 0 to List.Count - 1 do begin
            try
                Conv := StrToInt(List.Strings[i]);
            except
                on Exception do begin
                    Conv := -1;
                end;
            end;
            if InRange(Conv, 0, Self.Capacity) then begin
                Value := Reg.ReadString(List.Strings[i]);
                Reg.WriteString(IntToStr(Index), Value);
                Inc(Index);
            end else begin
                if Conv > Self.Capacity then begin //Retira entradas acima da capacidade
                    Reg.DeleteValue(List.Strings[i]);
                end;
            end;
        end;
    finally
        Reg.Free;
        List.Free;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
constructor TRegURL.Create;
begin
    inherited Create;
    FRegKey      := 'HKEY_CURRENT_USER\Software\ARS\Roger\URLCtrls' + Self.ClassName;
    Self.Capacity := 10;
    FCharCase    := ecNormal;
    FAllowBlanks := False;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
destructor TRegURL.Destroy;
begin
    Self.Commit;
    inherited Destroy;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegURL.ReadFromINIFile(const IniFile, IniSection : string) : boolean;
var
    Ini :      TIniFile;
    ListEntries, ListValues : TStringList;
    Reg :      TRegistryNT;
    i, Index : Integer;
begin
    if FileExists(IniFile) then begin
        Ini := TIniFile.Create(IniFile);
        ListEntries := TStringList.Create;
        ListValues := TStringList.Create;
        Reg := TRegistryNT.Create;
        try
            Reg.AccessMode := KEY_WRITE;
            Reg.OpenFullKey(FRegKey, True);
            Ini.ReadSection(IniSection, ListEntries);
            Ini.ReadSectionValues(IniSection, ListValues);
            Self.Clear;
            Index := -1;
            for i := 0 to (ListEntries.Count - 1) do begin
                try
                    Index := StrToInt(ListEntries.Strings[i]);
                except
                    on Exception do begin
                        System.Continue;
                    end;
                end;
                if Index <= Self.Capacity then begin
                    Reg.WriteString(IntToStr(Index), ListValues.Strings[i]);
                end;
            end;
        finally
            Ini.Free;
            ListEntries.Free;
            ListValues.Free;
            Reg.Free;
            Self.Compact;
        end;
        Result := True;
    end else begin
        Result := False;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
procedure TRegURL.Refresh;
var
    List :  TStringList;
    Reg :   TRegistryNT;
    i :     Integer;
    Value : string;
begin
    List := TStringList.Create;
    Reg  := TRegistryNT.Create;
    Reg.AccessMode := KEY_QUERY_VALUE;
    try
        Self.ReadItemsList(List);
        if Reg.OpenFullKey(Self.FRegKey, True) then begin
            i := Self.Capacity;
            Self.Clear;
            Self.Capacity := i;
            for i := 0 to MinIntValue([List.Count - 1, Self.Capacity]) do begin
                Value := Reg.ReadString(IntToStr(i));
                Self.Add(Value);
            end;
        end;
    finally
        List.Free;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TRegURL.SaveToINIFile(const IniFile, IniSection : PChar) : boolean;
var
    i : Integer;
begin
    //!!!!!! limpa a secao inicialmente
    if not WritePrivateProfileString(IniSection, nil, nil, IniFile) then begin
        for i := 0 to Self.Count - 1 do begin
            if (not AllowBlanks) and (Self.Strings[i] = EmptyStr) then begin
                System.Continue;
            end;
            if not WritePrivateProfileString(IniSection, PChar(IntToStr(i + 1)), PChar(Self.Strings[i]), IniFile) then begin
                Result := False;
                Exit;
            end;
        end;
        Result := True;
    end else begin
        Result := False;
    end;
end;

{-**********************************************************************
************************************************************************
******************
******************  Class:    TCtrlsHnd
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
class function TCtrlsHnd.ForceFocus(AControl : TWinControl) : boolean;
{{
Força o foco para um determinado controle.

AControl : Controle que se desejar forçar o foco.
}
var
    ParentControl : TWinControl;
begin
    Result := False; //controle ou pai inicialmente inviavel
    if (AControl.Enabled) then begin
        ParentControl := InvalidFocusControl(AControl);
        if ((ParentControl <> nil) and ParentControl.Enabled) then begin
            try
                //Verifica o caso de tabsheets ocultos
                if (ParentControl is TTabSheet) then begin
                    TTabSheet(ParentControl).PageControl.ActivePage := TTabSheet(ParentControl);
                end else begin
                    ParentControl.SetFocus();
                end;
            except
                Result := False;
                Exit;   //desista !
            end;
        end;
        if (AControl.Showing) then begin
            AControl.SetFocus;
        end else begin
            ParentControl := AControl.Parent;
            if ((ParentControl <> nil) and (ParentControl is TWinControl)) then begin
                ParentControl.Show;
                TCtrlsHnd.ForceFocus(ParentControl);
            end;
            if (AControl.Showing) then begin    //Ultima tentativa
                AControl.SetFocus();
            end;
        end;
        Application.ProcessMessages();
        Result := AControl.Focused;
    end;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
class function TCtrlsHnd.GotoNextControl(AControl : TWinControl; FwdDir : boolean = True; Execute : boolean = True) : TWinControl;
 {{
 Retorna o controle a ser focado para o caso de pressionamento de tab/(shift+tab) em relacão ao controle passado.

 AControl : Vem com o controle atualmente com o foco e volta com o controle alvo segundo a direcao passada

 FwdDdir : isso mesmo

 Execute : realiza a alteracao do foco

 }
var
    TargetControl : THackedWinControl;
begin
    Result := nil;
    if not Assigned(AControl) then begin
        Exit;
    end;
    TargetControl := THackedWinControl(AControl.Parent);  //Identifica o pai do controle de origem
    if not Assigned(TargetControl) then begin
        Result := nil;
        Exit;
    end;
    //Identifica o controle alvo
    TargetControl := THackedWinControl(TargetControl.FindNextControl(AControl, FwdDir, True, True));
    if Execute and (Assigned(TargetControl)) then begin
        SendMessage(AControl.Handle, WM_KILLFOCUS, 0, 0);
        TargetControl.SetFocus();
    end;


    //Forma alternativa
  {
  if TComponent(AControl).Owner <> nil then begin
    ParentComponent := TComponent(AControl).Owner.GetParentComponent;
    repeat begin
        ParentComponent := ParentComponent.GetParentComponent;
    end;
    until not ParentComponent.HasParent;
    if Execute then begin
        if FwdDir then begin
            SendMessage(TWinControl(ParentComponent).Handle, WM_NEXTDLGCTL, 0, 0);
        end else begin
            SendMessage(TWinControl(ParentComponent).Handle, WM_NEXTDLGCTL, -1, 0);
        end;
    end;
  end else begin
      AControl:=nil;
      Result:=-1;
  end;
  }
end;

class function TCtrlsHnd.InvalidFocusControl(AControl : TWinControl) : TWinControl;
{{
Retorna a referência do controle de mais alta hierarquia que impede que o controle dado receba o foco. Caso nenhum controle se
encontre nesta situação nil será retornado.

Revision: 18/10/2006 - Roger
}
var
    Q : TObjectStack;
begin
    Result := AControl;
    Q      := TObjectStack.Create;
    try
        //monta a pilha de controles
        while (Result <> nil) do begin
            Q.Push(Result);
            Result := Result.Parent;
        end;
        //na ordem inversa testa as condições
        while (Q.Count > 0) do begin
            Result := TWinControl(Q.Pop());
            if not (Result.Showing and Result.Enabled) then begin
                Exit;
            end;
        end;
    finally
        Q.Free;
    end;
end;

end.
