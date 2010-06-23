unit WinODBC;

interface

uses
	Windows, SysUtils;

type
	TWinODBC = Class
	public
		Class procedure CreateODBCDriver(const cDSNName, cDescription, cDataBase, cServer, cDriver: string; cLogon : string = 'no');
	end;

implementation


Class procedure TWinODBC.CreateODBCDriver(const cDSNName, cDescription, cDataBase, cServer, cDriver: string; cLogon : string = 'no');
{{
Cria um DSN no odbc da maquina em Runtime. Passando os seguintes parametros:
cDSNName : Nome do DSN.
cDescripition : Descriçao do DSN.
cDataBase : Nome da Base de Dados.
cServer : Nome ou IP do Servidor.
cDriver : Nome do Driver do SGDB a ser Utilizado.
cLogon : Flag de Autenticação de Usuário no Dominio ou no SGDB. Valores ("yes", "no") Referente ao Dominio.

Revision: 25/7/2005
}
type
	TSQLConfigDataSource = function(hwndParent : HWND; fRequest : WORD; lpszDriver : LPCSTR; lpszAttributes : LPCSTR) : BOOL; stdcall;
const
	ODBC_ADD_DSN = 1;
  {{
    Adiciona uma fonte de dados (data source).
  }
	ODBC_CONFIG_DSN = 2;
  {{
    Configura a fonte de dados (data source).
  }
	ODBC_REMOVE_DSN = 3;
  {{
   Remove a fonte de dados (data source).
  }
	ODBC_ADD_SYS_DSN = 4;
  {{
    Adiciona um DSN no sistema.
  }
	ODBC_CONFIG_SYS_DSN = 5;
  {{
    Configura o DSN do sistema.
  }
	ODBC_REMOVE_SYS_DSN = 6;
  {{
    Remove o DSN do sistema.
  }
	DEFAULT_PATH = 'ODBCCP32';
  {{
    Path da DLL a ser carregada. Considera o Diretório do Sistema Operacional como Default.
  }
	CONFIG_SQL = 'SQLConfigDataSource';
  {{
    Nome do método da DLL.
  }

var
	pFn : TSQLConfigDataSource;
	hLib : LongWord;
	strAttr : string;
	fResult : BOOL;
begin
	hLib := LoadLibraryA(PAnsiChar(DEFAULT_PATH)); // carregando para o diretório padrão
	if (hLib <> 0) then begin
		@pFn := GetProcAddress(hLib, PChar(CONFIG_SQL));
		if (@pFn <> NIL) then	begin
			strAttr := Format('DSN=%s' + #0 +
				'Server=%s' + #0 +
				'Description=%s' + #0 +
				'DATABASE=%s' + #0 +
				'Trusted_Connection=%s' + #0 + #0,
				[cDSNName, cServer, cDescription, cDataBase, cLogon]);
			fResult := pFn(0, ODBC_ADD_DSN , @cDriver[1], @strAttr[1]);
			if (fResult = FALSE) then begin
				raise exception.Create('Falha ao tentar criar o DSN (Data Source).' + #13 + 'Por favor Contacte o Desenvolvedor.');
			end;
			FreeLibrary(hLib);
		end else begin
			raise exception.create('O Sistema não pode Carregar a Biblioteca ODBCCP32.DLL.' + #13 + 'Por favor Contacte o Desenvolvedor.');
		end;
	end;
end;

end.


