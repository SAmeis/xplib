unit WEntity;

interface

uses
   Classes, SysUtils, HttpApp, HTTPProd;


type
   TWebState	=	( wsInactive, 	//Nenhuma pendencia existente
					  wsRedir,		//Foi gerado um desvio reaproveitando os dados anteriores
					  wsRequest,	//Lendo dados da requisicao
					  wsResponse	//Enviando dados de resposta
					);


   { TODO -oroger -cLIB : Criar editor de propriedades para WebFields e eventos do WebModule associados aos verbos }
   //*** Response e Request sao dinamicamente alterados durante o processo sempre trac pelo WModule


type
   TWebEntity  =   class( TComponent )
   private
       FCookiesParam : TStringList;
       FWebState : TWebState;
       FHTMLTemplateFile: string;
       FLastErrorCode: integer;
       FVerb: string;
       FLastErrorMessage: string;
       FSessionHandled: boolean;
       function GetRequest: TWebRequest;
       function GetResponse: TWebResponse;
       procedure SetVerb(const Value: string);
   protected
       procedure ClearWebFields;
       procedure RaiseErrorCode( ErrorCode : integer );
       procedure SaveCookies( Sender : TObject );
       procedure SetWebFieldsErrorValues;
   public
       property CookiesParam : TStringList read FCookiesParam ;
       property HTMLTemplateFile : string read FHTMLTemplateFile write FHTMLTemplateFile;
       constructor Create(AOwner: TComponent); override; //Owner = WebModule
       destructor Destroy; override;

       property LastErrorCode : integer read FLastErrorCode;
       property LastErrorMessage : string read FLastErrorMessage write FLastErrorMessage;
       property Request : TWebRequest read GetRequest;
       property Response : TWebResponse read GetResponse;
       property SessionHandled : boolean read FSessionHandled;
       property Verb : string read FVerb write SetVerb;

       function  ExecuteVerb( Verb : string; var Handled : boolean): integer; virtual;
       function InitSession( var Handled : boolean ) : integer; virtual; //Geralmente overrided
       procedure LoadWebFieldsByCookies( CkFields : TStrings ); virtual;
		procedure LoadWebFieldsByRequest( WR : TWebRequest ); virtual;
		procedure Reset; virtual;
       function  ResolveTag( Request : TWebRequest; TagString : string; Tag : TTag; TagParams: TStrings ) : string;
       procedure RedirectToURL( const URL : string );
   end;


implementation

{ TWebActor }

procedure TWebEntity.ClearWebFields;
begin

end;

constructor TWebEntity.Create(AOwner: TComponent);
begin
   inherited;
   FWebState:=wsInactive;
   //Checar sempre se o pai eh um WModule
end;

destructor TWebEntity.Destroy;
begin
  inherited;

end;

function TWebEntity.ExecuteVerb(Verb: string; var Handled: boolean): integer;
begin
//Criar propriedade Handled e cada metodo de verbo altera se necessitar de redirecao por exemplo
   Self.FSessionHandled:=Handled;
   //....
   Handled:=FSessionHandled;
   result:=0;
end;

function TWebEntity.GetRequest: TWebRequest;
begin
	result:=nil;
end;

function TWebEntity.GetResponse: TWebResponse;
begin
	result:=nil;
end;

function TWebEntity.InitSession(var Handled: boolean): integer;
begin
	result:=0;
end;

procedure TWebEntity.LoadWebFieldsByCookies(CkFields: TStrings);
begin
   //Geralmente overrided inherited ....
end;

procedure TWebEntity.LoadWebFieldsByRequest(WR: TWebRequest);
begin
{ TODO -oRoger -cLIB : necessita implementacao }
end;

procedure TWebEntity.RaiseErrorCode(ErrorCode: integer);
begin
{ TODO -oRoger -cLIB : necessita implementacao }
end;

procedure TWebEntity.RedirectToURL(const URL: string);
begin
   Self.FWebState:=wsRedir;
   Self.FSessionHandled:=False;
   //Response.Redirect( url );
end;

procedure TWebEntity.Reset;
begin
{ TODO -oRoger -cLIB : necessita implementacao }
end;

function TWebEntity.ResolveTag(Request: TWebRequest; TagString: string; Tag: TTag; TagParams: TStrings ) : string;
begin
{ TODO -oRoger -cLIB : necessita implementacao }
result:=EmptyStr;
end;

procedure TWebEntity.SaveCookies(Sender: TObject);
begin
{ TODO -oRoger -cLIB : necessita implementacao }
end;

procedure TWebEntity.SetVerb(const Value: string);
begin
  FVerb := Value;
end;

procedure TWebEntity.SetWebFieldsErrorValues;
begin
{ TODO -oRoger -cLIB : necessita implementacao }
end;

end.
