unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Grids, EnhGrids, ExtCtrls, ImgList,
  ComCtrls, StdCtrls, Buttons, Mask, ToolEdit, Operatrs, TreeHnd, Contnrs;

type
  TForm1 = class(TForm)
    EditExpPanel: TPanel;
    ExpTreeView: TTreeView;
    PageControl1: TPageControl;
    FunctionSheet: TTabSheet;
    OperatorsListView: TListView;
    VarSheet: TTabSheet;
    ConstantSheet: TTabSheet;
    Splitter1: TSplitter;
    BitBtn1: TBitBtn;
    LoadOperators: TButton;
    FilenameEdit: TFilenameEdit;
    OperDic: TOperatorDictionary;
    VarListView: TListView;
    PropVarBox: TGroupBox;
    RemVarBtn: TButton;
    AddVarBtn: TButton;
    Label1: TLabel;
    VarValueEdit: TEdit;
    VarTypeCombo: TComboBox;
    Label2: TLabel;
    procedure ExpTreeViewDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
    procedure ExpTreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);
    procedure LoadOperatorsClick(Sender: TObject);
    procedure OperDicUpdateItem(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure ExpTreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
    procedure ExpTreeViewKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure BitBtn1Click(Sender: TObject);
    procedure AddVarBtnClick(Sender: TObject);
    procedure VarListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure VarTypeComboChange(Sender: TObject);
    procedure VarValueEditChange(Sender: TObject);
    procedure VarListViewGetImageIndex(Sender: TObject; Item: TListItem);
  private
    { Private declarations }
    procedure GUIAddNode( Node : TStrTreeNode; Operation : TNodeOperation; var CanDo : boolean );
    procedure GUIRemoveNode( Node : TStrTreeNode; Operation : TNodeOperation; var CanDo : boolean );
    procedure GUIUpdateNode( Node : TStrTreeNode; Operation : TNodeOperation; var CanDo : boolean );
  protected
    function DropOperator( Node : TTreeNode; Operator : TVisualOperator ) : TVisualOperand;
    function DropVariable( Node: TTreeNode; Value : TVisualOperandValue ) : TVisualOperand;
  public
    { Public declarations }
    VarList : TObjectList;
    ConstList : TObjectList;
    constructor Create( AOwner : TComponent ); override;
    destructor Destroy; override;
  end;

var
  Form1: TForm1;

implementation

uses
   ImgLHnd, VCLHnd, TypInfo;

{$R *.DFM}

procedure TForm1.ExpTreeViewDragOver(Sender, Source: TObject; X, Y: Integer; State: TDragState; var Accept: Boolean);
//----------------------------------------------------------------------------------------------------------------------
var
   ht : THitTests;
   Node : TTreeNode;
begin
   Accept:=False;
   ht:=Self.ExpTreeView.GetHitTestInfoAt( X, Y );
   if ( ( htOnItem in ht ) or ( htOnStateIcon in ht ) ) and (
      ( ( TComponent( Source ).Name = 'OperatorsListView' ) or ( TComponent( Source ).Name = Self.VarListView.Name ) ) )then
   begin
       Node:=Self.ExpTreeView.GetNodeAt( x, y );
       Accept:=Assigned( Node );
   end;
end;

procedure TForm1.ExpTreeViewDragDrop(Sender, Source: TObject; X, Y: Integer);
//----------------------------------------------------------------------------------------------------------------------
var
   Node : TTreeNode;
   ListItem : TListItem;
   OldPosIndex : integer;
   Operand : TVisualOperand;
begin
   OldPosIndex:=-1;
   Node:=Self.ExpTreeView.GetNodeAt( X, Y );
   if Assigned( Node ) then begin
       Operand:=Node.Data;
       if Operand.Operator.OperatorType in [ otVariable, otConstant ] then begin
           if MessageDlg( 'Substituir valor ?', mtConfirmation, [ mbYes, mbNO ], 0 ) = mrYES then begin
               Node:=Node.Parent; //Salva o pai do atual para troca de posicao no final
               OldPosIndex:=Operand.ParentOperand.IndexOf( Operand );
               Operand.ParentOperand.Delete( OldPosIndex );
           end else begin
               Exit; //Cancela operacao
           end;
       end;
       ListItem:=TListView( Source ).Selected;  //Pode ser operador ou operando
       //Elemento vindo da lista de operadores
       if ( TComponent( Source ).Name = Self.OperatorsListView.Name ) then begin
           Operand:=Self.DropOperator( Node, ListItem.Data );
           Exit;
       end;
       //Elemento vindo da lista de variaveis
       if TComponent( Source ).Name = Self.VarListView.Name then begin
           Operand:=Self.DropVariable( Node, ListItem.Data );
           Exit;
       end;
       if ( OldPosIndex >= 0 ) and Assigned( Operand ) then begin  //Ajusta a posicao devido a reposicao
           { TODO -oRoger -cANALISE : Mover para a posicao original }
       end;
   end;
end;

procedure TForm1.LoadOperatorsClick(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
begin
   Self.OPerDic.LoadOperatorDictionary( Self.FilenameEdit.Text );
end;

procedure TForm1.OperDicUpdateItem(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
var
   i : integer;
   Oper : TVisualOperator;
   ListItem : TListItem;
begin
   //Atualiza a lista de operadores do dialogo
   Oper:=TVisualOperator( Sender );
   Self.OperatorsListView.Items.BeginUpdate();
   try
       if Assigned( Oper ) then begin
           ListItem:=nil;
           for i:=0 to Self.OperatorsListView.Items.Count-1 do begin
               if Self.OperatorsListView.Items[i].Data = Oper then begin
                   ListItem:=Self.OperatorsListView.Items[i];
                   Break;
               end;
           end;
           if not Assigned( ListItem ) then begin
               //Criar novo e adcionar a lista
               ListItem:=Self.OperatorsListView.Items.Add( );
           end;
           //Atualizar valores do item
           ListItem.Caption:=Oper.DisplayName;
           ListItem.ImageIndex:=Oper.ImageIndex;
           ListItem.Data:=Oper;
       end else begin
           //Renovar todos os elementos
           Self.OperatorsListView.Items.Clear();
           for i:=0 to Self.OperDic.OperatorCount-1 do begin
               if Self.OperDic.Operators[i].OperatorType in [otFunction..otOperator ] then begin //Remove estaticos
                   Self.OperDicUpdateItem( Self.OperDic.Operators[i] );
               end;
           end;
       end;
   finally
       Self.OperatorsListView.Items.EndUpdate();
   end;
end;

procedure TForm1.FormShow(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
var
   V : TVisualOperandValue;
   VItem : TListItem;
begin
   Self.LoadOperatorsClick( nil );  //Carga dos operadores padroes
   Self.VarListView.Items.BeginUpdate();
   try
       VItem:=Self.VarListView.Items.Add();
       V:=TVisualOperandValue.Create( Self.OperDic.VarOperator.DisplayName, Self.OperDic, nil );
       V.Name:='Var1';
       V.Value:='1';
       Self.VarList.Add( V );
       VItem.Caption:=V.Name;
       VItem.Data:=V;
       V.Data:=VItem;
       VItem:=Self.VarListView.Items.Add();
       V:=TVisualOperandValue.Create( Self.OperDic.VarOperator.DisplayName, Self.OperDic, nil );
       V.Name:='Var2';
       V.Value:='2';
       Self.VarList.Add( V );
       VItem.Caption:=V.Name;
       VItem.Data:=V;
       V.Data:=VItem;
   finally
       Self.VarListView.Items.EndUpdate();
   end;
end;

procedure TForm1.FormCreate(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
//Adciona o operando resultado como no raiz do treeview e carrega operadores padrao
var
   Operand : TVisualOperand;
   Node : TTreeNode;
begin
   Self.OperDic.InitializeDefaultOperators();  //Carrega operadores padrao
   Operand:=TVisualOperand.Create( Self.OperDic.ResultOperator.DisplayName, Self.OperDic, nil ); //Result Operator
   Operand.OperatorDictionary:=Self.OperDic; //Amarra todos nos filhos deste
   Node:=Self.ExpTreeView.Items.AddChild( nil, 'Resultado' );
   Operand.OnAddSubNode:=Self.GUIAddNode;
   Operand.OnRemoveSubNode:=Self.GUIRemoveNode;
   Operand.OnUpdateNode:=Self.GUIUpdateNode;
   Node.ImageIndex:=Operand.ImageIndex;
   Node.SelectedIndex:=Operand.ImageIndex;
   Node.Data:=Operand;
   Operand.Data:=Node;
end;

procedure TForm1.ExpTreeViewGetImageIndex(Sender: TObject; Node: TTreeNode);
//----------------------------------------------------------------------------------------------------------------------
begin
	Node.ImageIndex:=TVisualOperand( Node.Data ).ImageIndex;
   Node.SelectedIndex:=TVisualOperand( Node.Data ).ImageIndex;
end;

procedure TForm1.GUIAddNode(Node: TStrTreeNode; Operation: TNodeOperation; var CanDo: boolean);
//----------------------------------------------------------------------------------------------------------------------
//Reflete operacao no tree view
var
   ParentTreeNode, SubTreeNode : TTreeNode;
   Operand : TVisualOperand;
begin
   if Assigned( Node.ParentNode ) then begin
       ParentTreeNode:=TTreeNode(Node.ParentNode.Data);
   end else begin
       ParentTreeNode:=nil;
   end;
   if Assigned( ParentTreeNode ) then begin
       SubTreeNode:=TTreeView(ParentTreeNode.TreeView).Items.AddChild( ParentTreeNode, Node.Name );
       SubTreeNode.Data:=Node;
       Node.Data:=SubTreeNode;
       Node.OnAddSubNode:=Node.ParentNode.OnAddSubNode;
       Node.OnRemoveSubNode:=Node.ParentNode.OnRemoveSubNode;
       Node.OnUpdateNode:=Node.ParentNode.OnUpdateNode;
       Self.ExpTreeView.Refresh(); //Tentativa de atualizar nos do tree !!!!
       //Gera operandos padrao para este recem-criado
       Operand:=TVisualOperand(Node);
       Operand.CheckArguments();
   end else begin
       CanDo:=False;
   end;
end;

procedure TForm1.GUIRemoveNode(Node: TStrTreeNode; Operation: TNodeOperation; var CanDo: boolean);
//----------------------------------------------------------------------------------------------------------------------
//Reflete operacao no tree view
var
   TreeNode : TTreeNode;
   ANode : TVisualOperand;
begin
   TreeNode:=TTreeNode( Node.Data ); //Mata o TreeNode ( O VisualOperand sera destruido por seu pai )
   ANode:=TreeNode.Data;
   TreeNode.Free;
   ANode.Data:=nil;
end;

procedure TForm1.GUIUpdateNode(Node: TStrTreeNode; Operation: TNodeOperation; var CanDo: boolean);
//----------------------------------------------------------------------------------------------------------------------
//Reflete operacao no tree view
var
   TreeNode : TTreeNode;
begin
   TreeNode:=TTreeNode(Node.Data);
   TreeNode.Text:=TVisualOperand( Node ).DisplayName;
   TreeNode.Data:=Node;
end;

procedure TForm1.ExpTreeViewKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
//----------------------------------------------------------------------------------------------------------------------
var
   Operand, AParentOperand : TVisualOperand;
   Index : integer;
begin
	if Key = VK_DELETE then begin
		Key:=0;
		Operand:=TVisualOperand(Self.ExpTreeView.Selected.Data);
		if Operand.Operator.OperatorType <> otResult then begin
			AParentOperand:=Operand.ParentOperand;
			Index:=Operand.ParentOperand.IndexOf( Operand );
			Operand.ParentOperand.Delete( Index );
			AParentOperand.CheckArguments();
		end;
	end;
end;

procedure TForm1.BitBtn1Click(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
var
	Ret : string;
begin
   Ret:=TVisualOperand( Self.ExpTreeView.TopItem.Data ).AsText;
   MessageDlg( Ret , mtInformation, [ mbOK ], 0 );
end;

procedure TForm1.AddVarBtnClick(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
var
   VarName : string;
   ListItem : TListItem;
   OperandValue : TVisualOperandValue;
   NameConflict : boolean;
begin
   { TODO -oRoger -cDSG : Criar um operando para representar a variavel }
   VarName:=EmptyStr;
   NameConflict:=True;
   while NameConflict do begin
       if InputQuery( 'Nova variável', 'Nome da nova variável', VarName ) then begin
           NameConflict:=(Self.VarListView.FindCaption( 0, VarName, False, True, True ) <> nil);
           if NameConflict then begin
               MessageDlg( 'Nome duplicado', mtError, [ mbOK ], 0 );
               System.Continue;
           end;
           OperandValue:=TVisualOperandValue.Create( Self.OperDic.VarOperator.DisplayName, Self.OperDic, nil );
           ListItem:=Self.VarListView.Items.Add();
           OperandValue.Data:=ListItem;
           OperandValue.Name:=VarName;
           OperandValue.ReturnType:=[ortUnknown];
           ListItem.Data:=OperandValue;
           ListItem.Caption:=VarName;
           ListItem.ImageIndex:=Self.OperDic.VarOperator.ImageIndex;
       end else begin
           Break;
       end;
   end;
end;

procedure TForm1.VarListViewChange(Sender: TObject; Item: TListItem; Change: TItemChange);
//----------------------------------------------------------------------------------------------------------------------
var
   OperandValue : TVisualOperandValue;
   Ret : integer;
   P : PInteger;
   Rt : TOperatorArgumentType;
begin
   if csDestroying in Self.ComponentState then begin
       Exit;
   end;
   if Self.VarListView.Selected = nil then begin
       Self.VarListView.Selected:=Self.VarListView.ItemFocused;
   end;
   if Self.VarListView.Selected = nil then begin
       Self.VarTypeCombo.Enabled:=False;
       Self.VarValueEdit.Enabled:=False;
   end else begin
       Self.VarTypeCombo.Enabled:=True;
       Self.VarValueEdit.Enabled:=True;
   end;
   if Self.VarListView.Selected <> nil then begin
       OperandValue:=TVisualOperandValue(Self.VarListView.Selected.Data);
       Rt:=OperandValue.ReturnType;
       P:=@Rt;
       Ret:=P^;
       Ret:=VCLHnd.FirstSetValue( TypeInfo( TOperatorArgumentType ), TIntegerSet(Ret));
       Self.VarTypeCombo.ItemIndex:=Ret; { TODO -oRoger -cDSG : Pegar o primeiro tipo do conjunto }
       Self.VarValueEdit.Text:=OperandValue.Value;
   end else begin
       Self.VarValueEdit.Text:=EmptyStr;
       Self.VarTypeCombo.ItemIndex:=0;
   end;
end;

destructor TForm1.Destroy;
//----------------------------------------------------------------------------------------------------------------------
var
   i : integer;
begin
   TVisualOperand(Self.ExpTreeView.TopItem.Data).Free;  //Mata noh raiz e todos os outros pendentes
   Self.VarList.Free;
   Self.ConstList.Free;
   for i:=Self.VarListView.Items.Count-1 downto 0 do begin
       TVisualOperand( Self.VarListView.Items[i] ).Free;
   end;
   inherited;
end;

procedure TForm1.VarTypeComboChange(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
var
   OperandValue : TVisualOperandValue;
   P : POperatorArgumentType;
   V : integer;
begin
   if Self.VarListView.Selected = nil then begin
       Self.VarListView.Selected:=Self.VarListView.ItemFocused;
   end;
   if Self.VarListView.Selected <> nil then begin
       OperandValue:=TVisualOperandValue(Self.VarListView.Selected.Data);
       V:=Self.VarTypeCombo.ItemIndex+1;
       P:=@V;
       OperandValue.ReturnType:=P^;
   end;
end;

procedure TForm1.VarValueEditChange(Sender: TObject);
//----------------------------------------------------------------------------------------------------------------------
var
   OperandValue : TVisualOperandValue;
begin
   if Self.VarListView.Selected = nil then begin
       Self.VarListView.Selected:=Self.VarListView.ItemFocused;
   end;
   if Self.VarListView.Selected <> nil then begin
       OperandValue:=TVisualOperandValue(Self.VarListView.Selected.Data);
       OperandValue.Value:=Self.VarValueEdit.Text;
   end;
end;

constructor TForm1.Create(AOwner: TComponent);
//----------------------------------------------------------------------------------------------------------------------
begin
   inherited;
   Self.Varlist:=TObjectList.Create;
   Self.ConstList:=TObjectList.Create;
end;

procedure TForm1.VarListViewGetImageIndex(Sender: TObject; Item: TListItem);
//----------------------------------------------------------------------------------------------------------------------
var
   Operand : TVisualOperand;
begin
   if Assigned( Item.Data ) then begin
       Operand:=TVisualOperand( Item.Data );
       if Item.ImageIndex <> Operand.ImageIndex then begin
           Item.ImageIndex:=Operand.ImageIndex;
       end;
   end;
end;

function TForm1.DropOperator(Node: TTreeNode; Operator: TVisualOperator) : TVisualOperand;
//----------------------------------------------------------------------------------------------------------------------
//Implementa um drop de um operador, Retornando o operando adcionado
var
   ParentOperand : TVisualOperand;
begin
   Result:=nil;
   ParentOperand:=Node.Data;
   if ParentOperand.Operator.OperatorType <> otUnknown then begin  //Checa drop sobre operando nulo
       if Operator.AcceptArgumentReturnTypes( Operator.OperatorReturnType ) then begin
           //Adciona novo operand abaixo do no passado
           //ChangedOperand:=Node.Data; //Marca no a pegar o foco no final
           Result:=TVisualOperand.Create( Operator.DisplayName, Self.OperDic, ParentOperand );
       end else begin
           MessageDlg( 'Tipo de retorno incompatível com operação', mtError, [ mbOK ], 0 );
           Exit;
       end;
   end else begin
       //Alterar o tipo do operando no noh passado para o novo
       if Assigned( ParentOperand ) then begin
           if ParentOperand.AcceptArgumentReturnTypes( Operator.OperatorArgumentType ) then begin //Testa possibilidade
               Result:=ParentOperand;
               Result.Operator:=Operator;
               Result.CheckArguments();
               Node.Text:=Result.Operator.DisplayName; //atualiza texto do no treeview
           end else begin
               MessageDlg( 'Tipo de retorno incompatível com operação', mtError, [ mbOK ], 0 );
               Exit;
           end;
       end;
   end;
   Node:=TTreeNode(Result.Data);
   Node.Selected:=True;
   Node.Expand( False );
end;

function TForm1.DropVariable(Node: TTreeNode; Value: TVisualOperandValue) : TVisualOperand;
//----------------------------------------------------------------------------------------------------------------------
//Drop de uma variavel
var
   ParentOperand : TVisualOperand;
   TreeNode : TTreeNode;
begin
   Result:=nil;
   ParentOperand:=Node.Data;
   if ParentOperand.Operator.OperatorType <> otUnknown then begin //Checa drop sobre operando nulo
       if ParentOperand.AcceptArgumentReturnTypes( Value.ReturnType ) then begin
           Result:=TVisualOperandValue.Create( Value.Operator.DisplayName, Self.OperDic, ParentOperand );
           Result.Name:=Value.Name;
           Result.Value:=Value.Value;
           Node:=Result.Data;
       end else begin
           MessageDlg( 'Tipo de retorno incompatível com operação', mtError, [ mbOK ], 0 );
           Exit;
       end;
   end else begin
       //Alterar o tipo do operando no noh passado para o novo
       if Assigned( ParentOperand ) then begin
           if ParentOperand.AcceptArgumentReturnTypes( Value.ReturnType ) then begin //Testa possibilidade
               ParentOperand:=TVisualOperand(Node.Parent.Data); //Salva no pai
               ParentOperand.Delete( ParentOperand.IndexOf( Node.Data ) ); //Mata noh alvo
               Result:=TVisualOperandValue.Create( Value.OperatorDictionary.VarOperator.DisplayName, Value.OperatorDictionary, ParentOperand );
               Result.Name:=Value.Name;
               Result.Value:=Value.Value;
               Result.CheckArguments();
               Node:=Result.Data;
               Node.Text:=Result.DisplayName; //atualiza texto do no treeview
           end else begin
               MessageDlg( 'Tipo de retorno incompatível com operação', mtError, [ mbOK ], 0 );
               Exit;
           end;
       end;
   end;
   Node:=TTreeNode(Result.Data);
   Node.Text:=Result.DisplayName;
   Node.Selected:=True;
   Node.Expand( False );
end;

end.
