{$IFDEF XPImgDesign}
    {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}

unit XPImgDesign;

interface

uses
    Classes, DesignIntf, DesignEditors, TypInfo, VCLEditors, imgList, Graphics, Types;

type
    TXPDefaultImageIndexProperty = Class (TIntegerProperty, ICustomPropertyDrawing, ICustomPropertyListDrawing)
    protected
        function ImageList : TCustomImageList; virtual; abstract;
    public
        function GetAttributes : TPropertyAttributes; override;
        procedure GetValues(Proc : TGetStrProc); override;
        function GetValue : string; override;
        procedure SetValue(const Value : string); override;
        procedure ListMeasureWidth(const Value : string; ACanvas : TCanvas; var AWidth : Integer); virtual;
        procedure ListMeasureHeight(const Value : string; ACanvas : TCanvas; var AHeight : Integer); virtual;
        procedure ListDrawValue(const Value : string; ACanvas : TCanvas; const ARect : TRect; ASelected : Boolean); virtual;
        procedure PropDrawName(ACanvas : TCanvas; const ARect : TRect; ASelected : Boolean);
        procedure PropDrawValue(ACanvas : TCanvas; const ARect : TRect; ASelected : Boolean);
    end;

    TXPImageProperty = Class (TXPDefaultImageIndexProperty)
    protected
        function ImageList : TCustomImageList; override;
    end;

procedure Register;

implementation


uses
    Windows, Dialogs, SysUtils, Math;

procedure Register;
{{ Colocar registro dos tipos a serem editados pelo TXPImageProperty , ver exemplo abaixo }
begin
    //   RegisterComponents( 'DB Objects', [ TDBOset, TDBOContainer ] );
    //   RegisterPropertyEditor( TypeInfo(TImageIndex), TDBOSet, 'ImageIndex', TXPImageProperty );
end;

function TXPDefaultImageIndexProperty.GetAttributes : TPropertyAttributes;
begin
    Result := [paValueList, paSortList, paMultiSelect, paRevertable];
end;

function TXPDefaultImageIndexProperty.GetValue : string;
begin
    Result := intToStr(GetOrdValue);
end;

procedure TXPDefaultImageIndexProperty.SetValue(const Value : string);
var
    XValue : Integer;
begin
    try
        XValue := strToInt(Value);
        SetOrdValue(XValue);
    except
        inherited SetValue(Value);
    end;
end;

procedure TXPDefaultImageIndexProperty.GetValues(Proc : TGetStrProc);
var
    Tmp : TCustomImageList;
    I : Integer;
begin
    Tmp := ImageList;
    if Assigned(Tmp) then    begin
        for I := 0 to Tmp.Count - 1 do begin
            Proc(intToStr(I));
        end;
    end;
end;

procedure TXPDefaultImageIndexProperty.ListMeasureWidth(const Value : string; ACanvas : TCanvas; var AWidth : Integer);
var
    Tmp : TCustomImageList;
begin
    Tmp := ImageList;
    if Assigned(Tmp) then    begin
        AWidth := Tmp.Width + ACanvas.TextHeight(Value) + 4;
    end;
end;

procedure TXPDefaultImageIndexProperty.ListMeasureHeight(const Value : string; ACanvas : TCanvas; var AHeight : Integer);
var
    Tmp : TCustomImageList;
begin
    Tmp := ImageList;
    if Assigned(Tmp) then    begin
        AHeight := Max(Tmp.Height + 2, ACanvas.TextHeight(Value) + 2);
    end;
end;

procedure TXPDefaultImageIndexProperty.ListDrawValue(const Value : string; ACanvas :
    TCanvas; const ARect : TRect; ASelected : Boolean);
var
    Tmp : TCustomImageList;
    R : TRect;
begin
    DefaultPropertyListDrawValue(Value, ACanvas, ARect, ASelected);
    Tmp := ImageList;
    if Tmp <> NIL then  begin
        R := ARect;
        ACanvas.FillRect(ARect);
        Tmp.Draw(ACanvas, ARect.Left, ARect.Top, StrToInt(Value));
        OffsetRect(R, Tmp.Width + 2, 0);
        DrawText(ACanvas.Handle, PChar(Value), -1, R, 0);
    end;
end;

procedure TXPDefaultImageIndexProperty.PropDrawName(ACanvas : TCanvas;
    const ARect : TRect; ASelected : Boolean);
begin
    DefaultPropertyDrawName(Self, ACanvas, ARect);
end;

procedure TXPDefaultImageIndexProperty.PropDrawValue(ACanvas : TCanvas;
    const ARect : TRect; ASelected : Boolean);
var
    Tmp : TCustomImageList;
begin
    Tmp := ImageList;
    if (GetVisualValue <> '') and Assigned(Tmp) then    begin
        ListDrawValue(GetVisualValue, ACanvas, ARect, ASelected);
    end else begin
        DefaultPropertyDrawValue(Self, ACanvas, ARect);
    end;
end;

function TXPImageProperty.ImageList : TCustomImageList;
{
var
   Item : TDBOSet;
   Container : TDBOContainer;
}
begin
    Result := NIL;
    //Usar um dos modelos abaixo para ter acesso ao ImageList do component em edicao
    //   Item := TDBOContainer(TypInfo.GetObjectProp(GetComponent(0), 'Container'));
    //   Item := TDBOSet(GetComponent(0));
{
   if ( Item <> nil ) then begin
       Container:=Item.Container;
       if ( Container <> nil ) then begin
           Result:=Container.Images;
       end;
   end else begin
     MessageDlg( 'DBOContainer não ajustado.', mtError, [ mbOK ], 0 );
   end;
}
end;


end.


