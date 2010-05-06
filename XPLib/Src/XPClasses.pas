{$IFDEF XPClasses }
  {$DEFINE DEBUG_UNIT}
{$ENDIF}
{$I XPLib.inc}

unit XPClasses;

{{
Classes ancestrais e utilitárias comuns a XPLib
}
{1 Classes ancestrais e utilitárias comuns a XPLib }

interface

uses
    Classes, SysUtils, Windows;

type
    TInterfaceImplementorObject = Class (TInterfacedObject)
 {{
 Classe de suporte a instancias qe implementam e resolvem interfaces.
 
 As Classes descendentes de TInterfaceImplementorObject podem ser passadas como parametro nos métodos que pedem um ou mais das
 interfaces implementadas por seus descendentes.
 
 A liberação pelo mecanismo de interfaces ( _Release() ) da instancia ocorrerá apenas se o atributo Self.Owner for nulo( o
 construtor o seta para si mesmo - Self ). Isto permite criar uma instancia desta classe no blocos de inicialização de um Unit.
 }
    private
        FOwner : TObject;
    protected
        function _Release : Integer; stdcall;
    public
        constructor Create; virtual;
        property Owner : TObject read FOwner write FOwner;
  {{
  Responsável pela liberacao da instancia, geralmente ele mesmo ou outra.
  }
        {1 Responsável pela liberacao da instancia, geralmente ele mesmo ou outra. }
    end;

implementation

{ TInterfaceImplementorObject }

{-**********************************************************************
************************************************************************
******************
******************  Class:    TInterfaceImplementorObject
******************  Category: No category
******************
************************************************************************
************************************************************************}
{--------------------------------------------------------------------------------------------------------------------------------}
constructor TInterfaceImplementorObject.Create;
{{
Cria e seta o Owner desta instanca para si mesmo.
}
    {1 Cria e seta o Owner desta instanca para si mesmo. }
begin
    inherited;
    Self.FOwner := Self;
end;

{--------------------------------------------------------------------------------------------------------------------------------}
function TInterfaceImplementorObject._Release : Integer;
{{
Libera a instancia apenas se o Owner for nulo.
}
    {1 Libera a instancia apenas se o Owner for nulo. }
begin
	Result := InterlockedDecrement(FRefCount);
	if( ( Self.FOwner <> NIL) and ( Result = 0 ) )then begin
		Result:=-1;
	end else begin
		if (Result = 0) then begin
			Destroy;
		end;
	end;
end;

end.


