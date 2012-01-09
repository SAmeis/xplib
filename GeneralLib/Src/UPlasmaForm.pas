{$IFDEF UPlasmaForm}
	 {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I GeneralLib.inc}

//History
//Revision - 20111124 - roger
//Importada para pacote, reformatda e inseridas as diretivas de compilação apropriadas
//Removida toda a parte de suporte para Design de modo a torna o pacote compilavel diretamente

{TODO -oroger -clib : Continuar com a profilaxia desta unit/componente, remover warnings, etc}


{
===============================================================================================
TPlasmaForm v1.1
================================================================================================

Author: Audreyn Justus
        aj@base.com.br

Legal Notice:

	This component is FreeWare for personal use. No responsibilities taken whatsoever.
        If you will use it , please let me know; send me an e-mail!!!


Files : uPlasmaForm.pas      - code (TPlasmaForm component);
        uPlasmaForm.Dcr      - bitmap to the component palette;
        uPlasmaEd.pas/.dfm   - code/form used in the property editor;
        Demo.Dpr             - sample application;
        UDemo.pas/.dfm       - code/form of the sample application;
        read.txt             - this file;

* Important : The bitmap used in the sample application is from K-Jöfol,a great mp3 player.
You can find it on www.mp3.com.



===============================================================================================
TPlasmaform
===============================================================================================
  It's a component derived from TImage used to give any shape to a form. Just drop it on a form, select a background bitmap and click on the mask property to set the transparent area.

  Property
  ========

    Mask         : Click on the mask property and an editor  will appear. Click on the color you                                       want to be transparent and then on the first button. The editor will produce
                   a mask to be used by the PlasmaForm component. After the process just click
                   on the button Ok. Its all.

    Moveable     : Can the window be moved?

  Methods
  =======

    LoadMaskFromFile     : Load a mask. The file may be produced by de mask property editor.
    LoadMaskFromResource : Load a mask from a resource.

  Register
  ========
    To register the component on Delphi just install the UPlasmaForm.pas. Will be create a "Justus" tab and the component will appear there.

===============================================================================================
Property Editor for the Mask
===============================================================================================

   There is a property editor fro mthe Mask (it is on UPlasmaEd files).
   The four buttons that appear are :
   1.-Process - produce the mask;
   2.-Save    - save the mask to a file. Then it may be used by the method LoadMaskFromFile as a
                alternate way of use masks.
   3.-Ok      - apply the mask to the TPlasmaform edited.
   4.-Cancel  - cancel and return to the object inspector.

===============================================================================================
Versions
===============================================================================================

v1.0      :  01/12/98 - first version

v1.1      :  01/04/99 - LoadMaskFromResource method added
                        (sugestion by Konstantin Leonidov)
                      - Set the form Scaled property to false
					   - The mask is no more cleared when the bitmap changes
}

unit UPlasmaForm;
//version 1.1

{==============================================================================
v1.0      :  01/12/98 - first version

v1.1      :  01/04/99 - LoadMaskFromResource method added
                        (sugestion by Konstantin Leonidov)
                      - Set the form Scaled property to false
                      - The mask is no more cleared when the bitmap changes
==============================================================================}

interface

uses
    Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
    ExtCtrls, Buttons, StdCtrls, UPlasmaRegion;

type
    TPlasmaForm = class(TImage)
    private
        FMoveable : boolean;
        FMoveX :    Integer;
        FMoveY :    Integer;
        FRegion :   TPlasmaRegion;
        procedure RefreshForm;
        procedure PictureUpdate(Sender : TObject);
        procedure MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer); override;
        procedure MouseMove(Shift : TShiftState; X, Y : Integer); override;
    protected
        procedure SetRegion(Value : TPlasmaRegion);
        function GetRegion : TPlasmaRegion;
        procedure SetParent(Value : TWinControl); override;
    public
        constructor Create(Aowner : TComponent); override;
        destructor Destroy; override;
        procedure Loaded; override;
        procedure LoadMaskFromFile(Value : string);
        procedure LoadMaskFromResource(Instance : THandle; ResName : string);
    published
        property Mask : TPlasmaRegion read GetRegion write SetRegion;
        property Moveable : boolean read FMoveable write FMoveable;
    end;


implementation


{==============================================================================}
constructor TPlasmaForm.Create(Aowner : TComponent);
begin
    inherited Create(AOwner);
    Align     := alClient;
    AutoSize  := True;
    FRegion   := TPlasmaRegion.Create(Self);
    Picture.OnChange := PictureUpdate;
    FMoveable := True;
end;

destructor TPlasmaForm.Destroy;
begin
    FRegion.Free;
    inherited Destroy;
end;

procedure TPlasmaForm.Loaded;
begin
    inherited Loaded;
    RefreshForm;
end;

{==============================================================================}
procedure TPlasmaForm.RefreshForm;
begin
    if (not (csDesigning in ComponentState)) then begin
        SetWindowRgn(Parent.Handle, FRegion.Region, True);
    end;
end;

{==============================================================================}
procedure TPlasmaForm.PictureUpdate(Sender : TObject);
begin
    if not Picture.Bitmap.Empty then begin
        Parent.ClientWidth  := Picture.Width;
        Parent.ClientHeight := Picture.Height;
    end;
end;

procedure TPlasmaForm.SetParent(Value : TWinControl);
begin
    inherited SetParent(Value);
    if Value <> nil then begin
        if (Value is TForm) then begin
            TForm(Value).BorderStyle := bsNone;
            TForm(Value).Scaled      := False;
        end else begin
            raise Exception.Create('Please Drop on a Form');
        end;
    end;
end;

{==============================================================================}
procedure TPlasmaForm.SetRegion(Value : TPlasmaRegion);
begin
    if Value <> nil then begin
        FRegion := Value;
    end;
    RefreshForm;
end;

function TPlasmaForm.GetRegion : TPlasmaRegion;
begin
    Result := FRegion;
end;

{===============================================================================}
procedure TPlasmaForm.MouseDown(Button : TMouseButton; Shift : TShiftState; X, Y : Integer);
begin
    inherited MouseDown(Button, Shift, X, Y);
    FMoveX := x;
    FMoveY := y;
end;

procedure TPlasmaForm.MouseMove(Shift : TShiftState; X, Y : Integer);
begin
    inherited MouseMove(Shift, x, y);
    if (FMoveable) and (Shift = [ssLeft]) then begin
        Parent.Left := Parent.Left + (x - FMoveX);
        Parent.Top  := Parent.Top + (y - FMoveY);
        SendMessage(Parent.Handle, WM_PAINT, 0, 0);
    end;
end;

{==============================================================================}
procedure TPlasmaForm.LoadMaskFromFile(Value : string);
var
    Reader : TFileStream;
    Data :   PRgnData;
begin
    try
        Reader := TFileStream.Create(Value, fmOpenRead);
        GetMem(Data, Reader.Size);
        Reader.Read(Data^, Reader.Size);
        FRegion.Region := ExtCreateRegion(nil, Reader.Size, Data^);
        FreeMem(Data, Reader.Size);
        Reader.Free;
        RefreshForm;
    except
        raise Exception.Create('Error Loading Mask');
    end;
end;


//to use number-> resname = #number like #128;
procedure TPlasmaForm.LoadMaskFromResource(Instance : THandle; ResName : string);
var
    Reader : TResourceStream;
    Data :   PRgnData;
begin
    try
        Reader := TResourceStream.Create(Instance, ResName, RT_RCDATA);
        GetMem(Data, Reader.Size);
        Reader.Read(Data^, Reader.Size);
        FRegion.Region := ExtCreateRegion(nil, Reader.Size, Data^);
        FreeMem(Data, Reader.Size);
        Reader.Free;
        RefreshForm;
    except
        raise Exception.Create('Error Loading Mask form resource');
    end;
end;

{==============================================================================}
//Parte removida para GeneralLibDsgSupport
{==============================================================================}
end.
