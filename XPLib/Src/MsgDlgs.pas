 //Devido ao fato de Consts.pas ter sido compilada com IMPORTEDDATA OFF esta unit nao pode ser colcada em nenhum pacote
 //As posssives razoes sao ligadas ao smart link que deseja remover as referencias ao recursos nao referenciados 

{$IFDEF MsgDlgs}
{$A+,B-,C-,D+,E-,F-,G+,H+,I+,J+,K-,L+,M-,N+,O-,P+,Q-,R+,S-,T-,U-,V+,W-,X+,Y+,Z1}
{$DEBUGINFO ON}
{$ELSE}
{$A+,B-,C-,D-,E-,F-,G+,H+,I+,J+,K-,L-,M-,N+,O+,P+,Q+,R+,S-,T-,U-,V+,W-,X+,Y-,Z4}
{$ENDIF}

{$IMPORTEDDATA OFF}

unit MsgDlgs;

interface

uses
    SysUtils, Windows, Classes, Messages, Graphics, Dialogs, Consts, Controls, Forms;


function CreateMessageDialogEx(const Msg : string; DlgType : TMsgDlgType; Buttons : TMsgDlgButtons; DefButton : TMsgDlgBtn) : TForm;
function MsgDlg(Tile : PChar; const Msg : string; DlgType : TMsgDlgType; Buttons : TMsgDlgButtons;
    DefButton : TMsgDlgBtn) : integer;


implementation

uses
    StdCtrls, Grf_Hnd, Math, ExtCtrls;

var
    ButtonWidths : array[TMsgDlgBtn] of integer;  // initialized to zero
    ButtonCaptions : array[TMsgDlgBtn] of Pointer = (@SMsgDlgYes, @SMsgDlgNo, @SMsgDlgOK, @SMsgDlgCancel, @SMsgDlgAbort,
        @SMsgDlgRetry, @SMsgDlgIgnore, @SMsgDlgAll, @SMsgDlgNoToAll, @SMsgDlgYesToAll,
        @SMsgDlgHelp);
    IconIDs :      array[TMsgDlgType] of PChar = (IDI_EXCLAMATION, IDI_HAND, IDI_ASTERISK, IDI_QUESTION, nil);
    Captions :     array[TMsgDlgType] of Pointer = (@SMsgDlgWarning, @SMsgDlgError, @SMsgDlgInformation, @SMsgDlgConfirm, nil);
    ButtonNames :  array[TMsgDlgBtn] of string = ('Yes', 'No', 'OK', 'Cancel', 'Abort', 'Retry', 'Ignore', 'All', 'NoToAll',
        'YesToAll', 'Help');
    ModalResults : array[TMsgDlgBtn] of integer = (mrYes, mrNo, mrOk, mrCancel, mrAbort, mrRetry, mrIgnore, mrAll, mrNoToAll,
        mrYesToAll, 0);

 //----------------------------------------------------------------------------------------------------------------------
 //Tipo de Form padrao para dialogos
type
    TMessageFormTemplate = class(TForm)
    private
        Message : TLabel;
        procedure HelpButtonClick(Sender : TObject);
    protected
        procedure CustomKeyDown(Sender : TObject; var Key : word; Shift : TShiftState);
        procedure CustomOnShow(Sender : TObject);
        procedure WriteToClipBoard(Text : string);
        function GetFormText : string;
    public
        constructor CreateNew(AOwner : TComponent); reintroduce;
    end;



function MsgDlg(Tile : PChar; const Msg : string; DlgType : TMsgDlgType; Buttons : TMsgDlgButtons;
    DefButton : TMsgDlgBtn) : integer;
    //----------------------------------------------------------------------------------------------------------------------
var
    Dlg : TForm;
begin
    Dlg := CreateMessageDialogEx(Msg, DlgType, Buttons, DefButton);
    try
        if Assigned(Tile) then begin
            Dlg.Caption := Tile;
        end else begin
            Dlg.Caption := Application.Title;
        end;
        Result := Dlg.ShowModal;
    finally
        Dlg.Free;
    end;
end;


function CreateMessageDialogEx(const Msg : string; DlgType : TMsgDlgType; Buttons : TMsgDlgButtons; DefButton : TMsgDlgBtn) : TForm;
    //----------------------------------------------------------------------------------------------------------------------
const
    mcHorzMargin    = 8;
    mcVertMargin    = 8;
    mcHorzSpacing   = 10;
    mcVertSpacing   = 10;
    mcButtonWidth   = 50;
    mcButtonHeight  = 14;
    mcButtonSpacing = 4;
var
    DialogUnits : TPoint;
    HorzMargin, VertMargin, HorzSpacing, VertSpacing, ButtonWidth,
    ButtonHeight, ButtonSpacing, ButtonCount, ButtonGroupWidth,
    IconTextWidth, IconTextHeight, X, ALeft : integer;
    B, DefaultButton, CancelButton : TMsgDlgBtn;
    IconID :      PChar;
    TextRect :    TRect;
begin
    Result := TMessageFormTemplate.CreateNew(Application);   //Instancia Form vazio para apresentacao
    with Result do begin

        //Condiciona propriedades dependentes da aplicacao
        BiDiMode    := Application.BiDiMode;
        BorderStyle := bsDialog;
        Canvas.Font := Font;
        KeyPreview  := True;
        OnKeyDown   := TMessageFormTemplate(Result).CustomKeyDown;
        OnShow      := TMessageFormTemplate(Result).CustomOnShow;  //Evento para focar botao atribuido como padrao
        //Parametros das dimensoes
        DialogUnits := ExtentCharSizeAverage(Canvas);              //Proporcoes de texto para o form criado
        HorzMargin  := MulDiv(mcHorzMargin, DialogUnits.X, 4);
        VertMargin  := MulDiv(mcVertMargin, DialogUnits.Y, 8);
        HorzSpacing := MulDiv(mcHorzSpacing, DialogUnits.X, 4);
        VertSpacing := MulDiv(mcVertSpacing, DialogUnits.Y, 8);
        ButtonWidth := MulDiv(mcButtonWidth, DialogUnits.X, 4);

        for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do begin
            if B in Buttons then begin  //Botao presente na lista
                if ButtonWidths[B] = 0 then begin  //Verifica se tamanho do botao ainda nao inicializado
                    TextRect := Rect(0, 0, 0, 0);
                    Windows.DrawText(Canvas.handle, PChar(LoadResString(ButtonCaptions[B])), -1, TextRect,
                        DT_CALCRECT or DT_LEFT or DT_SINGLELINE or DrawTextBiDiModeFlagsReadingOnly);
                    with TextRect do begin
                        ButtonWidths[B] := Right - Left + 8;
                    end;
                end;
                if ButtonWidths[B] > ButtonWidth then begin
                    ButtonWidth := ButtonWidths[B];
                end;
            end;
        end;
        ButtonHeight  := MulDiv(mcButtonHeight, DialogUnits.Y, 8);
        ButtonSpacing := MulDiv(mcButtonSpacing, DialogUnits.X, 4);
        SetRect(TextRect, 0, 0, Screen.Width div 2, 0);
        DrawText(Canvas.Handle, PChar(Msg), Length(Msg) + 1, TextRect,
            DT_EXPANDTABS or DT_CALCRECT or DT_WORDBREAK or DrawTextBiDiModeFlagsReadingOnly);

        //Propriedades da imagem figurativo do dialogo
        IconID := IconIDs[DlgType];
        IconTextWidth := TextRect.Right;
        IconTextHeight := TextRect.Bottom;
        if IconID <> nil then begin
            Inc(IconTextWidth, 32 + HorzSpacing);
            if IconTextHeight < 32 then begin
                IconTextHeight := 32;
            end;
        end;

        ButtonCount := 0;
        for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do begin
            if B in Buttons then begin
                Inc(ButtonCount);
            end;
        end;
        ButtonGroupWidth := 0;
        if ButtonCount <> 0 then begin
            ButtonGroupWidth := ButtonWidth * ButtonCount + ButtonSpacing * (ButtonCount - 1);
        end;
        ClientWidth := Max(IconTextWidth, ButtonGroupWidth) + HorzMargin * 2;
        ClientHeight := IconTextHeight + ButtonHeight + VertSpacing + VertMargin * 2;
        Left := (Screen.Width div 2) - (Width div 2);
        Top  := (Screen.Height div 2) - (Height div 2);
        if DlgType <> mtCustom then begin
            Caption := LoadResString(Captions[DlgType]);
        end else begin
            Caption := Application.Title;
        end;
        if IconID <> nil then begin
            with TImage.Create(Result) do begin
                Name   := 'Image';
                Parent := Result;
                Picture.Icon.Handle := LoadIcon(0, IconID);
                SetBounds(HorzMargin, VertMargin, 32, 32);
            end;
        end;
        TMessageFormTemplate(Result).Message := TLabel.Create(Result);
        with TMessageFormTemplate(Result).Message do begin
            Name     := 'Message';
            Parent   := Result;
            WordWrap := True;
            Caption  := Msg;
            BoundsRect := TextRect;
            BiDiMode := Result.BiDiMode;
            ALeft    := IconTextWidth - TextRect.Right + HorzMargin;
            if UseRightToLeftAlignment then begin
                ALeft := Result.ClientWidth - ALeft - Width;
            end;
            SetBounds(ALeft, VertMargin, TextRect.Right, TextRect.Bottom);
        end;

        //Escolha do botao padrao para o ID_OK
        if DefButton in Buttons then begin
            DefaultButton := DefButton;
        end else begin
            if mbOK in Buttons then begin
                DefaultButton := mbOK;
            end else begin
                if mbYes in Buttons then begin
                    DefaultButton := mbYes;
                end else begin
                    DefaultButton := mbRetry;
                end;
            end;
        end;
        //Escolha do botao para o ID_CANCEL
        if mbCancel in Buttons then begin
            CancelButton := mbCancel;
        end else begin
            if mbNo in Buttons then begin
                CancelButton := mbNo;
            end else begin
                CancelButton := mbOK;
            end;
        end;
        X := (ClientWidth - ButtonGroupWidth) div 2;
        for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do begin
            if B in Buttons then begin
                with TButton.Create(Result) do begin
                    Name    := ButtonNames[B];
                    Parent  := Result;
                    Caption := LoadResString(ButtonCaptions[B]);
                    ModalResult := ModalResults[B];
                    if B = DefaultButton then begin
                        Default := True;
                    end;
                    if B = CancelButton then begin
                        Cancel := True;
                    end;
                    SetBounds(X, IconTextHeight + VertMargin + VertSpacing, ButtonWidth, ButtonHeight);
                    Inc(X, ButtonWidth + ButtonSpacing);
                    if B = mbHelp then begin
                        OnClick := TMessageFormTemplate(Result).HelpButtonClick;
                    end;
                end;
            end;
        end;
    end;
end;

{ TMessageFormTemplate }

constructor TMessageFormTemplate.CreateNew(AOwner : TComponent);
    //----------------------------------------------------------------------------------------------------------------------
var
    NonClientMetrics : TNonClientMetrics;
begin
    inherited CreateNew(AOwner);
    NonClientMetrics.cbSize := sizeof(NonClientMetrics);
    if SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @NonClientMetrics, 0) then begin
        Font.Handle := CreateFontIndirect(NonClientMetrics.lfMessageFont);
    end;
end;

procedure TMessageFormTemplate.CustomKeyDown(Sender : TObject; var Key : word; Shift : TShiftState);
//----------------------------------------------------------------------------------------------------------------------
begin
    if (Shift = [ssCtrl]) and (Key = word('C')) then begin
        SysUtils.Beep;
        WriteToClipBoard(GetFormText);
    end;
end;

procedure TMessageFormTemplate.CustomOnShow(Sender : TObject);
//----------------------------------------------------------------------------------------------------------------------
var
    i :   integer;
    Btn : TButton;
begin
    for i := 0 to Self.ComponentCount - 1 do begin
        if Self.Components[i] is TButton then begin
            Btn := TButton(Self.Components[i]);
            if Btn.Default then begin
                if Btn.CanFocus then begin
                    Btn.SetFocus();
                end;
            end;
        end;
    end;
end;

function TMessageFormTemplate.GetFormText : string;
    //----------------------------------------------------------------------------------------------------------------------
var
    DividerLine, ButtonCaptions : string;
    I : integer;
begin
    DividerLine := StringOfChar('-', 27) + sLineBreak;
    for I := 0 to ComponentCount - 1 do begin
        if Components[I] is TButton then begin
            ButtonCaptions := ButtonCaptions + TButton(Components[I]).Caption + StringOfChar(' ', 3);
        end;
    end;
    ButtonCaptions := StringReplace(ButtonCaptions, '&', '', [rfReplaceAll]);
    Result := Format('%s%s%s%s%s%s%s%s%s%s',
        [DividerLine, Caption, sLineBreak, DividerLine, Message.Caption, sLineBreak,
        DividerLine, ButtonCaptions, sLineBreak, DividerLine]);
end;

procedure TMessageFormTemplate.HelpButtonClick(Sender : TObject);
//----------------------------------------------------------------------------------------------------------------------
begin
    Application.HelpContext(HelpContext);
end;

procedure TMessageFormTemplate.WriteToClipBoard(Text : string);
//----------------------------------------------------------------------------------------------------------------------
var
    Data :    THandle;
    DataPtr : Pointer;
begin
    if OpenClipBoard(0) then begin
        try
            Data := GlobalAlloc(GMEM_MOVEABLE + GMEM_DDESHARE, Length(Text) + 1);
            try
                DataPtr := GlobalLock(Data);
                try
                    Move(PChar(Text)^, DataPtr^, Length(Text) + 1);
                    EmptyClipBoard;
                    SetClipboardData(CF_TEXT, Data);
                finally
                    GlobalUnlock(Data);
                end;
            except
                GlobalFree(Data);
                raise;
            end;
        finally
            CloseClipBoard;
        end;
    end else begin
        raise Exception.CreateRes(@SCannotOpenClipboard);
    end;
end;

end.
