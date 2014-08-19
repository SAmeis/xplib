unit wmiUtils;

interface

uses
	ActiveX, ComObj;

///  <summary>
///    Instancia interface de automacao OLE para os serviços WMI, ou outro tipo de objeto registrado na automação
///  </summary>
///  <remarks>
///
///  </remarks>
function WMIGetObject(const Name : string) : IDispatch;

implementation


function WMIGetObject(const Name : string) : IDispatch;
///  <summary>
///    Instancia interface de automacao OLE para os serviços WMI, ou outro tipo de objeto registrado na automação
///  </summary>
///  <remarks>
///
///  </remarks>
var
	 Moniker :  IMoniker;
	 Eaten :    Integer;
	 BindContext : IBindCtx;
	 Dispatch : IDispatch;
begin
	 OleCheck(CreateBindCtx(0, BindContext));
	 OleCheck(MkParseDisplayName(BindContext, PWideChar(WideString(Name)), Eaten, Moniker));
	 OleCheck(Moniker.BindToObject(BindContext, nil, IDispatch, Dispatch));
	 Result := Dispatch;
end;

end.
