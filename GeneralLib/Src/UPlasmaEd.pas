{$IFDEF UPlasmaEd}
	 {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I GeneralLib.inc}

unit UPlasmaEd;

interface

uses
	 Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
	 ExtCtrls, Buttons, StdCtrls, DesignEditors, DesignIntf;

type

    TPlasmaRegion = class(TPersistent)
    private
        FRegion : Hrgn;
        FOwner :  TImage;
        procedure SetRegion(Value : HRgn);
    protected
        procedure ReadMask(Reader : TStream);
        procedure WriteMask(Writer : TStream);
        procedure DefineProperties(Filer : TFiler); override;
    public
        procedure SetRegionByData(Value : PRgnData; Size : Integer);
        constructor Create(AOwner : TImage);
        destructor Destroy; override;
        property Owner : TImage read FOwner;
        property Region : HRgn read FRegion write SetRegion;
    end;

    TPlasmaMaskProperty = class(TPropertyEditor)
    private
        function GetAttributes : TPropertyAttributes; override;
        function GetValue : string; override;
        procedure Edit; override;
    end;

    TMaskForm = class(TForm)
        HPanel :     TPanel;
        VPanel :     TPanel;
        Image :      TImage;
        ColorPanel : TPanel;
        Label1 :     TLabel;
        BtGo :       TSpeedButton;
        BtOk :       TSpeedButton;
        BtCancel :   TSpeedButton;
        BtSave :     TSpeedButton;
        SaveDialog : TSaveDialog;
        procedure ImageMouseDown(Sender : TObject; Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
        procedure BtOkClick(Sender : TObject);
        procedure BtCancelClick(Sender : TObject);
        procedure BtGoClick(Sender : TObject);
        procedure BtSaveClick(Sender : TObject);
        procedure FormCreate(Sender : TObject);
        procedure FormDestroy(Sender : TObject);
    private
        FRegion : TPlasmaRegion;
        procedure SetBitmap(Value : TBitmap);
        procedure SetRegion(Value : HRgn);
        function GetRegion : HRgn;
    public
        procedure Empty;
        property Region : HRgn read GetRegion write SetRegion;
        property Bitmap : TBitmap write SetBitmap;
    end;


implementation

{$R *.DFM}

{===========================TPlasmaRegion======================================}
constructor TPlasmaRegion.Create(AOwner : TImage);
begin
    inherited Create;
    FOwner := AOwner;
end;

destructor TPlasmaRegion.Destroy;
begin
    if FRegion <> 0 then begin
        DeleteObject(FRegion);
    end;
    inherited Destroy;
end;

procedure TPlasmaRegion.SetRegionByData(Value : PRgnData; Size : Integer);
begin
    if FRegion <> 0 then begin
        DeleteObject(FRegion);
    end;
    if Value = nil then begin
        FRegion := 0;
    end else begin
        FRegion := ExtCreateRegion(nil, Size, Value^);
    end;
end;

procedure TPlasmaRegion.SetRegion(Value : HRgn);
var
    p : PRgnData;
    s : Integer;
begin
    if FRegion <> 0 then begin
        DeleteObject(FRegion);
    end;
    if Value = 0 then begin
        FRegion := 0;
    end else begin
        s := GetRegionData(Value, 0, nil);
        GetMem(p, s);
        GetRegionData(Value, s, p);
        FRegion := ExtCreateRegion(nil, s, p^);
        FreeMem(p, s);
    end;
end;

procedure TPlasmaRegion.ReadMask(Reader : TStream);
var
    Data : PRgnData;
    Size : Integer;
begin
    Reader.Read(Size, Sizeof(Size));
    if Size > 0 then begin
        GetMem(Data, Size);
        Reader.Read(Data^, Size);
        SetRegionByData(Data, Size);
        FreeMem(Data, Size);
    end else begin
        Region := 0;
    end;
end;

procedure TPlasmaRegion.WriteMask(Writer : TStream);
var
    Size : Integer;
    Data : PRgnData;
begin
    Size := GetRegionData(Region, 0, nil);
    Writer.Write(Size, SizeOf(Size));
    if (Size > 0) then begin
        GetMem(Data, Size);
        GetRegionData(Region, Size, Data);
        Writer.Write(Data^, Size);
        FreeMem(Data, Size);
    end;
end;

procedure TPlasmaRegion.DefineProperties(Filer : TFiler);
begin
    inherited DefineProperties(Filer);
    Filer.DefineBinaryProperty('Data', ReadMask, WriteMask, True);
end;

{==========================Property Editor=====================================}
function TPlasmaMaskProperty.GetAttributes : TPropertyAttributes;
begin
    Result := inherited GetAttributes + [paDialog];
end;

function TPlasmaMaskProperty.GetValue : string;
begin
    if TPlasmaRegion(GetOrdValue).Region = 0 then begin
        Result := '(None)';
    end else begin
        Result := '(Mask)';
    end;
end;

procedure TPlasmaMaskProperty.Edit;
var
    Mf : TMaskForm;
begin
    Mf := TMaskForm.Create(Application);
    Mf.Bitmap := TPlasmaRegion(GetOrdvalue).Owner.Picture.Bitmap;
    Mf.Region := TPlasmaRegion(GetOrdvalue).Region;
    if Mf.ShowModal = mrOk then begin
        TPlasmaRegion(GetOrdvalue).Region := Mf.Region;
        Modified;
    end;
    Mf.Free;
end;

{================================Form==========================================}
procedure TMaskForm.ImageMouseDown(Sender : TObject; Button : TMouseButton;
    Shift : TShiftState; X, Y : Integer);
begin
    ColorPanel.Color := Image.Picture.Bitmap.Canvas.Pixels[x, y];
end;

procedure TMaskForm.Empty;
begin
    FRegion.Region := 0;
    BtOK.Enabled   := False;
    BtSave.Enabled := False;
end;

procedure TMaskForm.SetRegion(Value : HRgn);
begin
    BtSave.Enabled := True;
    BtOk.Enabled   := True;
    FRegion.Region := Value;
end;

function TMaskForm.GetRegion : HRgn;
begin
    Result := FRegion.Region;
end;

procedure TMaskForm.SetBitmap(Value : TBitmap);
begin
    Empty;
    Image.Picture.Bitmap.Assign(Value);
    if not Value.Empty then begin
        ClientWidth      := VPanel.Width + Value.Width;
        ClientHeight     := HPanel.Height + Value.Height;
        ColorPanel.Color := Value.Canvas.Pixels[0, 0];
        BtGo.Enabled     := True;
    end else begin
        ClientWidth  := 250;
        ClientHeight := 220;
    end;
    Application.ProcessMessages;
end;

procedure TMaskForm.BtOkClick(Sender : TObject);
begin
    ModalResult := mrOk;
end;

procedure TMaskForm.BtCancelClick(Sender : TObject);
begin
    ModalResult := mrCancel;
end;

procedure TMaskForm.BtGoClick(Sender : TObject);
var
    RStart : Integer;
    REnd :   Integer;
    i, j :   Integer;

    procedure PaintRegion(x1, x2, y : Integer; Color1, Color2 : TColor);
    var
        k : Integer;
    begin
        for k := x1 to x2 do begin
            if (Odd(k div 7)) xor (Odd(y div 7)) then begin
                Image.Picture.Bitmap.Canvas.Pixels[k, y] := Color1;
            end else begin
                Image.Picture.Bitmap.Canvas.Pixels[k, y] := Color2;
            end;
        end;
        Application.ProcessMessages;
    end;

    procedure AddRegion(x1, x2, y : Integer);
    var
        Aux : HRgn;
    begin
        if FRegion.Region = 0 then begin
            FRegion.Region := CreateRectRgn(x1, y, x2 + 1, y + 1);
        end else begin
            Aux := CreateRectRgn(x1, y, x2 + 1, y + 1);
            CombineRgn(FRegion.Region, FRegion.Region, Aux, RGN_OR);
            DeleteObject(Aux);
        end;
    end;

begin
    BtGo.Enabled   := False;
    FRegion.Region := 0;
    Application.ProcessMessages;
    for j := 0 to Image.Picture.Height - 1 do begin
        RStart := -1;
        REnd   := -1;
        for i := 0 to Image.Picture.Width - 1 do begin
            if ((Image.Picture.Bitmap.Canvas.Pixels[i, j] <> ColorPanel.Color) or
                (i = Image.Picture.Width - 1)) and
                (RStart = -1) then begin
                RStart := i;
                PaintRegion(REnd + 1, RStart - 1, j, clSilver, clWhite);
            end;
            if ((Image.Picture.Bitmap.Canvas.Pixels[i, j] = ColorPanel.Color) or
                (i = Image.Picture.Width - 1)) and
                (RStart <> -1) then begin
                REnd := i - 1;
                PaintRegion(RStart, REnd, j, clRed, clYellow);
                AddRegion(RStart, REnd, j);
                RStart := -1;
            end;
        end;
    end;
    BtSave.Enabled := True;
    BtOk.Enabled   := True;
    ShowMessage('Ok, is Finished');
end;

procedure TMaskForm.BtSaveClick(Sender : TObject);
var
    F : TFileStream;
    p : PRgnData;
    s : Integer;
begin
    if FRegion.Region <> 0 then begin
        if SaveDialog.Execute then begin
            F := TFileStream.Create(SaveDialog.FileName, fmCreate);
            s := GetRegionData(FRegion.Region, 0, nil);
            GetMem(p, s);
            GetRegionData(FRegion.Region, s, p);
            F.Write(p^, s);
            FreeMem(p, s);
            F.Free;
            BtSave.Enabled := False;
        end;
    end;
end;

procedure TMaskForm.FormCreate(Sender : TObject);
begin
    FRegion := TPlasmaRegion.Create(Image);
end;

procedure TMaskForm.FormDestroy(Sender : TObject);
begin
    FRegion.Free;
end;

{==============================================================================}

end.
