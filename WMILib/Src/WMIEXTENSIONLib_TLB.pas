unit WMIEXTENSIONLib_TLB;

// ************************************************************************ //
// WARNING                                                                    
// -------                                                                    
// The types declared in this file were generated from data read from a       
// Type Library. If this type library is explicitly or indirectly (via        
// another type library referring to this type library) re-imported, or the   
// 'Refresh' command of the Type Library Editor activated while editing the   
// Type Library, the contents of this file will be regenerated and all        
// manual modifications will be lost.                                         
// ************************************************************************ //

// $Rev: 17244 $
// File generated on 31/3/2011 18:29:14 from Type Library described below.

// ************************************************************************  //
// Type Lib: C:\WINDOWS\system32\wbem\wbemads.tlb (1)
// LIBID: {E503D000-5C7F-11D2-8B74-00104B2AFB41}
// LCID: 0
// Helpfile: 
// HelpString: WMI Extension to DS 1.0 Type Library
// DepndLst: 
//   (1) v2.0 stdole, (C:\WINDOWS\system32\stdole2.tlb)
// Errors:
//   Hint: Member 'Class' of 'ISWbemObjectPath' changed to 'Class_'
//   Error creating palette bitmap of (TWMIExtension) : Server C:\WINDOWS\system32\wbem\wbemads.dll contains no icons
// ************************************************************************ //
// *************************************************************************//
// NOTE:                                                                      
// Items guarded by $IFDEF_LIVE_SERVER_AT_DESIGN_TIME are used by properties  
// which return objects that may need to be explicitly created via a function 
// call prior to any access via the property. These items have been disabled  
// in order to prevent accidental use from within the object inspector. You   
// may enable them by defining LIVE_SERVER_AT_DESIGN_TIME or by selectively   
// removing them from the $IFDEF blocks. However, such items must still be    
// programmatically created via a method of the appropriate CoClass before    
// they can be used.                                                          
{$TYPEDADDRESS OFF} // Unit must be compiled without type-checked pointers. 
{$WARN SYMBOL_PLATFORM OFF}
{$WRITEABLECONST ON}
{$VARPROPSETTER ON}
{$ALIGN 4}
interface

uses Windows, ActiveX, Classes, Graphics, OleServer, StdVCL, Variants;
  

// *********************************************************************//
// GUIDS declared in the TypeLibrary. Following prefixes are used:        
//   Type Libraries     : LIBID_xxxx                                      
//   CoClasses          : CLASS_xxxx                                      
//   DISPInterfaces     : DIID_xxxx                                       
//   Non-DISP interfaces: IID_xxxx                                        
// *********************************************************************//
const
  // TypeLibrary Major and minor versions
  WMIEXTENSIONLibMajorVersion = 1;
  WMIEXTENSIONLibMinorVersion = 0;

  LIBID_WMIEXTENSIONLib: TGUID = '{E503D000-5C7F-11D2-8B74-00104B2AFB41}';

  IID_IWMIExtension: TGUID = '{ADC1F06E-5C7E-11D2-8B74-00104B2AFB41}';
  IID_ISWbemObject: TGUID = '{76A6415A-CB41-11D1-8B02-00600806D9B6}';
  IID_ISWbemObjectPath: TGUID = '{5791BC27-CE9C-11D1-97BF-0000F81E849C}';
  IID_ISWbemNamedValueSet: TGUID = '{CF2376EA-CE8C-11D1-8B05-00600806D9B6}';
  IID_ISWbemNamedValue: TGUID = '{76A64164-CB41-11D1-8B02-00600806D9B6}';
  IID_ISWbemSecurity: TGUID = '{B54D66E6-2287-11D2-8B33-00600806D9B6}';
  IID_ISWbemPrivilegeSet: TGUID = '{26EE67BF-5804-11D2-8B4A-00600806D9B6}';
  IID_ISWbemPrivilege: TGUID = '{26EE67BD-5804-11D2-8B4A-00600806D9B6}';
  IID_ISWbemObjectSet: TGUID = '{76A6415F-CB41-11D1-8B02-00600806D9B6}';
  IID_ISWbemQualifierSet: TGUID = '{9B16ED16-D3DF-11D1-8B08-00600806D9B6}';
  IID_ISWbemQualifier: TGUID = '{79B05932-D3B7-11D1-8B06-00600806D9B6}';
  IID_ISWbemPropertySet: TGUID = '{DEA0A7B2-D4BA-11D1-8B09-00600806D9B6}';
  IID_ISWbemProperty: TGUID = '{1A388F98-D4BA-11D1-8B09-00600806D9B6}';
  IID_ISWbemMethodSet: TGUID = '{C93BA292-D955-11D1-8B09-00600806D9B6}';
  IID_ISWbemMethod: TGUID = '{422E8E90-D955-11D1-8B09-00600806D9B6}';
  IID_ISWbemServices: TGUID = '{76A6415C-CB41-11D1-8B02-00600806D9B6}';
  IID_ISWbemEventSource: TGUID = '{27D54D92-0EBE-11D2-8B22-00600806D9B6}';
  CLASS_WMIExtension: TGUID = '{F0975AFE-5C7F-11D2-8B74-00104B2AFB41}';

// *********************************************************************//
// Declaration of Enumerations defined in Type Library                    
// *********************************************************************//
// Constants for enum WbemImpersonationLevelEnum
type
  WbemImpersonationLevelEnum = TOleEnum;
const
  wbemImpersonationLevelAnonymous = $00000001;
  wbemImpersonationLevelIdentify = $00000002;
  wbemImpersonationLevelImpersonate = $00000003;
  wbemImpersonationLevelDelegate = $00000004;

// Constants for enum WbemAuthenticationLevelEnum
type
  WbemAuthenticationLevelEnum = TOleEnum;
const
  wbemAuthenticationLevelDefault = $00000000;
  wbemAuthenticationLevelNone = $00000001;
  wbemAuthenticationLevelConnect = $00000002;
  wbemAuthenticationLevelCall = $00000003;
  wbemAuthenticationLevelPkt = $00000004;
  wbemAuthenticationLevelPktIntegrity = $00000005;
  wbemAuthenticationLevelPktPrivacy = $00000006;

// Constants for enum WbemPrivilegeEnum
type
  WbemPrivilegeEnum = TOleEnum;
const
  wbemPrivilegeCreateToken = $00000001;
  wbemPrivilegePrimaryToken = $00000002;
  wbemPrivilegeLockMemory = $00000003;
  wbemPrivilegeIncreaseQuota = $00000004;
  wbemPrivilegeMachineAccount = $00000005;
  wbemPrivilegeTcb = $00000006;
  wbemPrivilegeSecurity = $00000007;
  wbemPrivilegeTakeOwnership = $00000008;
  wbemPrivilegeLoadDriver = $00000009;
  wbemPrivilegeSystemProfile = $0000000A;
  wbemPrivilegeSystemtime = $0000000B;
  wbemPrivilegeProfileSingleProcess = $0000000C;
  wbemPrivilegeIncreaseBasePriority = $0000000D;
  wbemPrivilegeCreatePagefile = $0000000E;
  wbemPrivilegeCreatePermanent = $0000000F;
  wbemPrivilegeBackup = $00000010;
  wbemPrivilegeRestore = $00000011;
  wbemPrivilegeShutdown = $00000012;
  wbemPrivilegeDebug = $00000013;
  wbemPrivilegeAudit = $00000014;
  wbemPrivilegeSystemEnvironment = $00000015;
  wbemPrivilegeChangeNotify = $00000016;
  wbemPrivilegeRemoteShutdown = $00000017;
  wbemPrivilegeUndock = $00000018;
  wbemPrivilegeSyncAgent = $00000019;
  wbemPrivilegeEnableDelegation = $0000001A;
  wbemPrivilegeManageVolume = $0000001B;

// Constants for enum WbemCimtypeEnum
type
  WbemCimtypeEnum = TOleEnum;
const
  wbemCimtypeSint8 = $00000010;
  wbemCimtypeUint8 = $00000011;
  wbemCimtypeSint16 = $00000002;
  wbemCimtypeUint16 = $00000012;
  wbemCimtypeSint32 = $00000003;
  wbemCimtypeUint32 = $00000013;
  wbemCimtypeSint64 = $00000014;
  wbemCimtypeUint64 = $00000015;
  wbemCimtypeReal32 = $00000004;
  wbemCimtypeReal64 = $00000005;
  wbemCimtypeBoolean = $0000000B;
  wbemCimtypeString = $00000008;
  wbemCimtypeDatetime = $00000065;
  wbemCimtypeReference = $00000066;
  wbemCimtypeChar16 = $00000067;
  wbemCimtypeObject = $0000000D;

type

// *********************************************************************//
// Forward declaration of types defined in TypeLibrary                    
// *********************************************************************//
  IWMIExtension = interface;
  IWMIExtensionDisp = dispinterface;
  ISWbemObject = interface;
  ISWbemObjectDisp = dispinterface;
  ISWbemObjectPath = interface;
  ISWbemObjectPathDisp = dispinterface;
  ISWbemNamedValueSet = interface;
  ISWbemNamedValueSetDisp = dispinterface;
  ISWbemNamedValue = interface;
  ISWbemNamedValueDisp = dispinterface;
  ISWbemSecurity = interface;
  ISWbemSecurityDisp = dispinterface;
  ISWbemPrivilegeSet = interface;
  ISWbemPrivilegeSetDisp = dispinterface;
  ISWbemPrivilege = interface;
  ISWbemPrivilegeDisp = dispinterface;
  ISWbemObjectSet = interface;
  ISWbemObjectSetDisp = dispinterface;
  ISWbemQualifierSet = interface;
  ISWbemQualifierSetDisp = dispinterface;
  ISWbemQualifier = interface;
  ISWbemQualifierDisp = dispinterface;
  ISWbemPropertySet = interface;
  ISWbemPropertySetDisp = dispinterface;
  ISWbemProperty = interface;
  ISWbemPropertyDisp = dispinterface;
  ISWbemMethodSet = interface;
  ISWbemMethodSetDisp = dispinterface;
  ISWbemMethod = interface;
  ISWbemMethodDisp = dispinterface;
  ISWbemServices = interface;
  ISWbemServicesDisp = dispinterface;
  ISWbemEventSource = interface;
  ISWbemEventSourceDisp = dispinterface;

// *********************************************************************//
// Declaration of CoClasses defined in Type Library                       
// (NOTE: Here we map each CoClass to its Default Interface)              
// *********************************************************************//
  WMIExtension = IWMIExtension;


// *********************************************************************//
// Declaration of structures, unions and aliases.                         
// *********************************************************************//
  POleVariant1 = ^OleVariant; {*}


// *********************************************************************//
// Interface: IWMIExtension
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {ADC1F06E-5C7E-11D2-8B74-00104B2AFB41}
// *********************************************************************//
  IWMIExtension = interface(IDispatch)
    ['{ADC1F06E-5C7E-11D2-8B74-00104B2AFB41}']
    function Get_WMIObjectPath: WideString; safecall;
    function GetWMIObject: ISWbemObject; safecall;
    function GetWMIServices: ISWbemServices; safecall;
    property WMIObjectPath: WideString read Get_WMIObjectPath;
  end;

// *********************************************************************//
// DispIntf:  IWMIExtensionDisp
// Flags:     (4416) Dual OleAutomation Dispatchable
// GUID:      {ADC1F06E-5C7E-11D2-8B74-00104B2AFB41}
// *********************************************************************//
  IWMIExtensionDisp = dispinterface
    ['{ADC1F06E-5C7E-11D2-8B74-00104B2AFB41}']
    property WMIObjectPath: WideString readonly dispid 1;
    function GetWMIObject: ISWbemObject; dispid 2;
    function GetWMIServices: ISWbemServices; dispid 3;
  end;

// *********************************************************************//
// Interface: ISWbemObject
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {76A6415A-CB41-11D1-8B02-00600806D9B6}
// *********************************************************************//
  ISWbemObject = interface(IDispatch)
    ['{76A6415A-CB41-11D1-8B02-00600806D9B6}']
    function Put_(iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObjectPath; safecall;
    procedure PutAsync_(const objWbemSink: IDispatch; iFlags: Integer; 
                        const objWbemNamedValueSet: IDispatch; const objWbemAsyncContext: IDispatch); safecall;
    procedure Delete_(iFlags: Integer; const objWbemNamedValueSet: IDispatch); safecall;
    procedure DeleteAsync_(const objWbemSink: IDispatch; iFlags: Integer; 
                           const objWbemNamedValueSet: IDispatch; 
                           const objWbemAsyncContext: IDispatch); safecall;
    function Instances_(iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; safecall;
    procedure InstancesAsync_(const objWbemSink: IDispatch; iFlags: Integer; 
                              const objWbemNamedValueSet: IDispatch; 
                              const objWbemAsyncContext: IDispatch); safecall;
    function Subclasses_(iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; safecall;
    procedure SubclassesAsync_(const objWbemSink: IDispatch; iFlags: Integer; 
                               const objWbemNamedValueSet: IDispatch; 
                               const objWbemAsyncContext: IDispatch); safecall;
    function Associators_(const strAssocClass: WideString; const strResultClass: WideString; 
                          const strResultRole: WideString; const strRole: WideString; 
                          bClassesOnly: WordBool; bSchemaOnly: WordBool; 
                          const strRequiredAssocQualifier: WideString; 
                          const strRequiredQualifier: WideString; iFlags: Integer; 
                          const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; safecall;
    procedure AssociatorsAsync_(const objWbemSink: IDispatch; const strAssocClass: WideString; 
                                const strResultClass: WideString; const strResultRole: WideString; 
                                const strRole: WideString; bClassesOnly: WordBool; 
                                bSchemaOnly: WordBool; const strRequiredAssocQualifier: WideString; 
                                const strRequiredQualifier: WideString; iFlags: Integer; 
                                const objWbemNamedValueSet: IDispatch; 
                                const objWbemAsyncContext: IDispatch); safecall;
    function References_(const strResultClass: WideString; const strRole: WideString; 
                         bClassesOnly: WordBool; bSchemaOnly: WordBool; 
                         const strRequiredQualifier: WideString; iFlags: Integer; 
                         const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; safecall;
    procedure ReferencesAsync_(const objWbemSink: IDispatch; const strResultClass: WideString; 
                               const strRole: WideString; bClassesOnly: WordBool; 
                               bSchemaOnly: WordBool; const strRequiredQualifier: WideString; 
                               iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                               const objWbemAsyncContext: IDispatch); safecall;
    function ExecMethod_(const strMethodName: WideString; const objWbemInParameters: IDispatch; 
                         iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObject; safecall;
    procedure ExecMethodAsync_(const objWbemSink: IDispatch; const strMethodName: WideString; 
                               const objWbemInParameters: IDispatch; iFlags: Integer; 
                               const objWbemNamedValueSet: IDispatch; 
                               const objWbemAsyncContext: IDispatch); safecall;
    function Clone_: ISWbemObject; safecall;
    function GetObjectText_(iFlags: Integer): WideString; safecall;
    function SpawnDerivedClass_(iFlags: Integer): ISWbemObject; safecall;
    function SpawnInstance_(iFlags: Integer): ISWbemObject; safecall;
    function CompareTo_(const objWbemObject: IDispatch; iFlags: Integer): WordBool; safecall;
    function Get_Qualifiers_: ISWbemQualifierSet; safecall;
    function Get_Properties_: ISWbemPropertySet; safecall;
    function Get_Methods_: ISWbemMethodSet; safecall;
    function Get_Derivation_: OleVariant; safecall;
    function Get_Path_: ISWbemObjectPath; safecall;
    function Get_Security_: ISWbemSecurity; safecall;
    property Qualifiers_: ISWbemQualifierSet read Get_Qualifiers_;
    property Properties_: ISWbemPropertySet read Get_Properties_;
    property Methods_: ISWbemMethodSet read Get_Methods_;
    property Derivation_: OleVariant read Get_Derivation_;
    property Path_: ISWbemObjectPath read Get_Path_;
    property Security_: ISWbemSecurity read Get_Security_;
  end;

// *********************************************************************//
// DispIntf:  ISWbemObjectDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {76A6415A-CB41-11D1-8B02-00600806D9B6}
// *********************************************************************//
  ISWbemObjectDisp = dispinterface
    ['{76A6415A-CB41-11D1-8B02-00600806D9B6}']
    function Put_(iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObjectPath; dispid 1;
    procedure PutAsync_(const objWbemSink: IDispatch; iFlags: Integer; 
                        const objWbemNamedValueSet: IDispatch; const objWbemAsyncContext: IDispatch); dispid 2;
    procedure Delete_(iFlags: Integer; const objWbemNamedValueSet: IDispatch); dispid 3;
    procedure DeleteAsync_(const objWbemSink: IDispatch; iFlags: Integer; 
                           const objWbemNamedValueSet: IDispatch; 
                           const objWbemAsyncContext: IDispatch); dispid 4;
    function Instances_(iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 5;
    procedure InstancesAsync_(const objWbemSink: IDispatch; iFlags: Integer; 
                              const objWbemNamedValueSet: IDispatch; 
                              const objWbemAsyncContext: IDispatch); dispid 6;
    function Subclasses_(iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 7;
    procedure SubclassesAsync_(const objWbemSink: IDispatch; iFlags: Integer; 
                               const objWbemNamedValueSet: IDispatch; 
                               const objWbemAsyncContext: IDispatch); dispid 8;
    function Associators_(const strAssocClass: WideString; const strResultClass: WideString; 
                          const strResultRole: WideString; const strRole: WideString; 
                          bClassesOnly: WordBool; bSchemaOnly: WordBool; 
                          const strRequiredAssocQualifier: WideString; 
                          const strRequiredQualifier: WideString; iFlags: Integer; 
                          const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 9;
    procedure AssociatorsAsync_(const objWbemSink: IDispatch; const strAssocClass: WideString; 
                                const strResultClass: WideString; const strResultRole: WideString; 
                                const strRole: WideString; bClassesOnly: WordBool; 
                                bSchemaOnly: WordBool; const strRequiredAssocQualifier: WideString; 
                                const strRequiredQualifier: WideString; iFlags: Integer; 
                                const objWbemNamedValueSet: IDispatch; 
                                const objWbemAsyncContext: IDispatch); dispid 10;
    function References_(const strResultClass: WideString; const strRole: WideString; 
                         bClassesOnly: WordBool; bSchemaOnly: WordBool; 
                         const strRequiredQualifier: WideString; iFlags: Integer; 
                         const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 11;
    procedure ReferencesAsync_(const objWbemSink: IDispatch; const strResultClass: WideString; 
                               const strRole: WideString; bClassesOnly: WordBool; 
                               bSchemaOnly: WordBool; const strRequiredQualifier: WideString; 
                               iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                               const objWbemAsyncContext: IDispatch); dispid 12;
    function ExecMethod_(const strMethodName: WideString; const objWbemInParameters: IDispatch; 
                         iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObject; dispid 13;
    procedure ExecMethodAsync_(const objWbemSink: IDispatch; const strMethodName: WideString; 
                               const objWbemInParameters: IDispatch; iFlags: Integer; 
                               const objWbemNamedValueSet: IDispatch; 
                               const objWbemAsyncContext: IDispatch); dispid 14;
    function Clone_: ISWbemObject; dispid 15;
    function GetObjectText_(iFlags: Integer): WideString; dispid 16;
    function SpawnDerivedClass_(iFlags: Integer): ISWbemObject; dispid 17;
    function SpawnInstance_(iFlags: Integer): ISWbemObject; dispid 18;
    function CompareTo_(const objWbemObject: IDispatch; iFlags: Integer): WordBool; dispid 19;
    property Qualifiers_: ISWbemQualifierSet readonly dispid 20;
    property Properties_: ISWbemPropertySet readonly dispid 21;
    property Methods_: ISWbemMethodSet readonly dispid 22;
    property Derivation_: OleVariant readonly dispid 23;
    property Path_: ISWbemObjectPath readonly dispid 24;
    property Security_: ISWbemSecurity readonly dispid 25;
  end;

// *********************************************************************//
// Interface: ISWbemObjectPath
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {5791BC27-CE9C-11D1-97BF-0000F81E849C}
// *********************************************************************//
  ISWbemObjectPath = interface(IDispatch)
    ['{5791BC27-CE9C-11D1-97BF-0000F81E849C}']
    function Get_Path: WideString; safecall;
    procedure Set_Path(const strPath: WideString); safecall;
    function Get_RelPath: WideString; safecall;
    procedure Set_RelPath(const strRelPath: WideString); safecall;
    function Get_Server: WideString; safecall;
    procedure Set_Server(const strServer: WideString); safecall;
    function Get_Namespace: WideString; safecall;
    procedure Set_Namespace(const strNamespace: WideString); safecall;
    function Get_ParentNamespace: WideString; safecall;
    function Get_DisplayName: WideString; safecall;
    procedure Set_DisplayName(const strDisplayName: WideString); safecall;
    function Get_Class_: WideString; safecall;
    procedure Set_Class_(const strClass: WideString); safecall;
    function Get_IsClass: WordBool; safecall;
    procedure SetAsClass; safecall;
    function Get_IsSingleton: WordBool; safecall;
    procedure SetAsSingleton; safecall;
    function Get_Keys: ISWbemNamedValueSet; safecall;
    function Get_Security_: ISWbemSecurity; safecall;
    function Get_Locale: WideString; safecall;
    procedure Set_Locale(const strLocale: WideString); safecall;
    function Get_Authority: WideString; safecall;
    procedure Set_Authority(const strAuthority: WideString); safecall;
    property Path: WideString read Get_Path write Set_Path;
    property RelPath: WideString read Get_RelPath write Set_RelPath;
    property Server: WideString read Get_Server write Set_Server;
    property Namespace: WideString read Get_Namespace write Set_Namespace;
    property ParentNamespace: WideString read Get_ParentNamespace;
    property DisplayName: WideString read Get_DisplayName write Set_DisplayName;
    property Class_: WideString read Get_Class_ write Set_Class_;
    property IsClass: WordBool read Get_IsClass;
    property IsSingleton: WordBool read Get_IsSingleton;
    property Keys: ISWbemNamedValueSet read Get_Keys;
    property Security_: ISWbemSecurity read Get_Security_;
    property Locale: WideString read Get_Locale write Set_Locale;
    property Authority: WideString read Get_Authority write Set_Authority;
  end;

// *********************************************************************//
// DispIntf:  ISWbemObjectPathDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {5791BC27-CE9C-11D1-97BF-0000F81E849C}
// *********************************************************************//
  ISWbemObjectPathDisp = dispinterface
    ['{5791BC27-CE9C-11D1-97BF-0000F81E849C}']
    property Path: WideString dispid 0;
    property RelPath: WideString dispid 1;
    property Server: WideString dispid 2;
    property Namespace: WideString dispid 3;
    property ParentNamespace: WideString readonly dispid 4;
    property DisplayName: WideString dispid 5;
    property Class_: WideString dispid 6;
    property IsClass: WordBool readonly dispid 7;
    procedure SetAsClass; dispid 8;
    property IsSingleton: WordBool readonly dispid 9;
    procedure SetAsSingleton; dispid 10;
    property Keys: ISWbemNamedValueSet readonly dispid 11;
    property Security_: ISWbemSecurity readonly dispid 12;
    property Locale: WideString dispid 13;
    property Authority: WideString dispid 14;
  end;

// *********************************************************************//
// Interface: ISWbemNamedValueSet
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {CF2376EA-CE8C-11D1-8B05-00600806D9B6}
// *********************************************************************//
  ISWbemNamedValueSet = interface(IDispatch)
    ['{CF2376EA-CE8C-11D1-8B05-00600806D9B6}']
    function Get__NewEnum: IUnknown; safecall;
    function Item(const strName: WideString; iFlags: Integer): ISWbemNamedValue; safecall;
    function Get_Count: Integer; safecall;
    function Add(const strName: WideString; var varValue: OleVariant; iFlags: Integer): ISWbemNamedValue; safecall;
    procedure Remove(const strName: WideString; iFlags: Integer); safecall;
    function Clone: ISWbemNamedValueSet; safecall;
    procedure DeleteAll; safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  ISWbemNamedValueSetDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {CF2376EA-CE8C-11D1-8B05-00600806D9B6}
// *********************************************************************//
  ISWbemNamedValueSetDisp = dispinterface
    ['{CF2376EA-CE8C-11D1-8B05-00600806D9B6}']
    property _NewEnum: IUnknown readonly dispid -4;
    function Item(const strName: WideString; iFlags: Integer): ISWbemNamedValue; dispid 0;
    property Count: Integer readonly dispid 1;
    function Add(const strName: WideString; var varValue: OleVariant; iFlags: Integer): ISWbemNamedValue; dispid 2;
    procedure Remove(const strName: WideString; iFlags: Integer); dispid 3;
    function Clone: ISWbemNamedValueSet; dispid 4;
    procedure DeleteAll; dispid 5;
  end;

// *********************************************************************//
// Interface: ISWbemNamedValue
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {76A64164-CB41-11D1-8B02-00600806D9B6}
// *********************************************************************//
  ISWbemNamedValue = interface(IDispatch)
    ['{76A64164-CB41-11D1-8B02-00600806D9B6}']
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(var varValue: OleVariant); safecall;
    function Get_Name: WideString; safecall;
    property Name: WideString read Get_Name;
  end;

// *********************************************************************//
// DispIntf:  ISWbemNamedValueDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {76A64164-CB41-11D1-8B02-00600806D9B6}
// *********************************************************************//
  ISWbemNamedValueDisp = dispinterface
    ['{76A64164-CB41-11D1-8B02-00600806D9B6}']
    function Value: OleVariant; dispid 0;
    property Name: WideString readonly dispid 2;
  end;

// *********************************************************************//
// Interface: ISWbemSecurity
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {B54D66E6-2287-11D2-8B33-00600806D9B6}
// *********************************************************************//
  ISWbemSecurity = interface(IDispatch)
    ['{B54D66E6-2287-11D2-8B33-00600806D9B6}']
    function Get_ImpersonationLevel: WbemImpersonationLevelEnum; safecall;
    procedure Set_ImpersonationLevel(iImpersonationLevel: WbemImpersonationLevelEnum); safecall;
    function Get_AuthenticationLevel: WbemAuthenticationLevelEnum; safecall;
    procedure Set_AuthenticationLevel(iAuthenticationLevel: WbemAuthenticationLevelEnum); safecall;
    function Get_Privileges: ISWbemPrivilegeSet; safecall;
    property ImpersonationLevel: WbemImpersonationLevelEnum read Get_ImpersonationLevel write Set_ImpersonationLevel;
    property AuthenticationLevel: WbemAuthenticationLevelEnum read Get_AuthenticationLevel write Set_AuthenticationLevel;
    property Privileges: ISWbemPrivilegeSet read Get_Privileges;
  end;

// *********************************************************************//
// DispIntf:  ISWbemSecurityDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {B54D66E6-2287-11D2-8B33-00600806D9B6}
// *********************************************************************//
  ISWbemSecurityDisp = dispinterface
    ['{B54D66E6-2287-11D2-8B33-00600806D9B6}']
    property ImpersonationLevel: WbemImpersonationLevelEnum dispid 1;
    property AuthenticationLevel: WbemAuthenticationLevelEnum dispid 2;
    property Privileges: ISWbemPrivilegeSet readonly dispid 3;
  end;

// *********************************************************************//
// Interface: ISWbemPrivilegeSet
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {26EE67BF-5804-11D2-8B4A-00600806D9B6}
// *********************************************************************//
  ISWbemPrivilegeSet = interface(IDispatch)
    ['{26EE67BF-5804-11D2-8B4A-00600806D9B6}']
    function Get__NewEnum: IUnknown; safecall;
    function Item(iPrivilege: WbemPrivilegeEnum): ISWbemPrivilege; safecall;
    function Get_Count: Integer; safecall;
    function Add(iPrivilege: WbemPrivilegeEnum; bIsEnabled: WordBool): ISWbemPrivilege; safecall;
    procedure Remove(iPrivilege: WbemPrivilegeEnum); safecall;
    procedure DeleteAll; safecall;
    function AddAsString(const strPrivilege: WideString; bIsEnabled: WordBool): ISWbemPrivilege; safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  ISWbemPrivilegeSetDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {26EE67BF-5804-11D2-8B4A-00600806D9B6}
// *********************************************************************//
  ISWbemPrivilegeSetDisp = dispinterface
    ['{26EE67BF-5804-11D2-8B4A-00600806D9B6}']
    property _NewEnum: IUnknown readonly dispid -4;
    function Item(iPrivilege: WbemPrivilegeEnum): ISWbemPrivilege; dispid 0;
    property Count: Integer readonly dispid 1;
    function Add(iPrivilege: WbemPrivilegeEnum; bIsEnabled: WordBool): ISWbemPrivilege; dispid 2;
    procedure Remove(iPrivilege: WbemPrivilegeEnum); dispid 3;
    procedure DeleteAll; dispid 4;
    function AddAsString(const strPrivilege: WideString; bIsEnabled: WordBool): ISWbemPrivilege; dispid 5;
  end;

// *********************************************************************//
// Interface: ISWbemPrivilege
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {26EE67BD-5804-11D2-8B4A-00600806D9B6}
// *********************************************************************//
  ISWbemPrivilege = interface(IDispatch)
    ['{26EE67BD-5804-11D2-8B4A-00600806D9B6}']
    function Get_IsEnabled: WordBool; safecall;
    procedure Set_IsEnabled(bIsEnabled: WordBool); safecall;
    function Get_Name: WideString; safecall;
    function Get_DisplayName: WideString; safecall;
    function Get_Identifier: WbemPrivilegeEnum; safecall;
    property IsEnabled: WordBool read Get_IsEnabled write Set_IsEnabled;
    property Name: WideString read Get_Name;
    property DisplayName: WideString read Get_DisplayName;
    property Identifier: WbemPrivilegeEnum read Get_Identifier;
  end;

// *********************************************************************//
// DispIntf:  ISWbemPrivilegeDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {26EE67BD-5804-11D2-8B4A-00600806D9B6}
// *********************************************************************//
  ISWbemPrivilegeDisp = dispinterface
    ['{26EE67BD-5804-11D2-8B4A-00600806D9B6}']
    property IsEnabled: WordBool dispid 0;
    property Name: WideString readonly dispid 1;
    property DisplayName: WideString readonly dispid 2;
    property Identifier: WbemPrivilegeEnum readonly dispid 3;
  end;

// *********************************************************************//
// Interface: ISWbemObjectSet
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {76A6415F-CB41-11D1-8B02-00600806D9B6}
// *********************************************************************//
  ISWbemObjectSet = interface(IDispatch)
    ['{76A6415F-CB41-11D1-8B02-00600806D9B6}']
    function Get__NewEnum: IUnknown; safecall;
    function Item(const strObjectPath: WideString; iFlags: Integer): ISWbemObject; safecall;
    function Get_Count: Integer; safecall;
    function Get_Security_: ISWbemSecurity; safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
    property Security_: ISWbemSecurity read Get_Security_;
  end;

// *********************************************************************//
// DispIntf:  ISWbemObjectSetDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {76A6415F-CB41-11D1-8B02-00600806D9B6}
// *********************************************************************//
  ISWbemObjectSetDisp = dispinterface
    ['{76A6415F-CB41-11D1-8B02-00600806D9B6}']
    property _NewEnum: IUnknown readonly dispid -4;
    function Item(const strObjectPath: WideString; iFlags: Integer): ISWbemObject; dispid 0;
    property Count: Integer readonly dispid 1;
    property Security_: ISWbemSecurity readonly dispid 4;
  end;

// *********************************************************************//
// Interface: ISWbemQualifierSet
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {9B16ED16-D3DF-11D1-8B08-00600806D9B6}
// *********************************************************************//
  ISWbemQualifierSet = interface(IDispatch)
    ['{9B16ED16-D3DF-11D1-8B08-00600806D9B6}']
    function Get__NewEnum: IUnknown; safecall;
    function Item(const Name: WideString; iFlags: Integer): ISWbemQualifier; safecall;
    function Get_Count: Integer; safecall;
    function Add(const strName: WideString; var varVal: OleVariant; 
                 bPropagatesToSubclass: WordBool; bPropagatesToInstance: WordBool; 
                 bIsOverridable: WordBool; iFlags: Integer): ISWbemQualifier; safecall;
    procedure Remove(const strName: WideString; iFlags: Integer); safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  ISWbemQualifierSetDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {9B16ED16-D3DF-11D1-8B08-00600806D9B6}
// *********************************************************************//
  ISWbemQualifierSetDisp = dispinterface
    ['{9B16ED16-D3DF-11D1-8B08-00600806D9B6}']
    property _NewEnum: IUnknown readonly dispid -4;
    function Item(const Name: WideString; iFlags: Integer): ISWbemQualifier; dispid 0;
    property Count: Integer readonly dispid 1;
    function Add(const strName: WideString; var varVal: OleVariant; 
                 bPropagatesToSubclass: WordBool; bPropagatesToInstance: WordBool; 
                 bIsOverridable: WordBool; iFlags: Integer): ISWbemQualifier; dispid 2;
    procedure Remove(const strName: WideString; iFlags: Integer); dispid 3;
  end;

// *********************************************************************//
// Interface: ISWbemQualifier
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {79B05932-D3B7-11D1-8B06-00600806D9B6}
// *********************************************************************//
  ISWbemQualifier = interface(IDispatch)
    ['{79B05932-D3B7-11D1-8B06-00600806D9B6}']
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(var varValue: OleVariant); safecall;
    function Get_Name: WideString; safecall;
    function Get_IsLocal: WordBool; safecall;
    function Get_PropagatesToSubclass: WordBool; safecall;
    procedure Set_PropagatesToSubclass(bPropagatesToSubclass: WordBool); safecall;
    function Get_PropagatesToInstance: WordBool; safecall;
    procedure Set_PropagatesToInstance(bPropagatesToInstance: WordBool); safecall;
    function Get_IsOverridable: WordBool; safecall;
    procedure Set_IsOverridable(bIsOverridable: WordBool); safecall;
    function Get_IsAmended: WordBool; safecall;
    property Name: WideString read Get_Name;
    property IsLocal: WordBool read Get_IsLocal;
    property PropagatesToSubclass: WordBool read Get_PropagatesToSubclass write Set_PropagatesToSubclass;
    property PropagatesToInstance: WordBool read Get_PropagatesToInstance write Set_PropagatesToInstance;
    property IsOverridable: WordBool read Get_IsOverridable write Set_IsOverridable;
    property IsAmended: WordBool read Get_IsAmended;
  end;

// *********************************************************************//
// DispIntf:  ISWbemQualifierDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {79B05932-D3B7-11D1-8B06-00600806D9B6}
// *********************************************************************//
  ISWbemQualifierDisp = dispinterface
    ['{79B05932-D3B7-11D1-8B06-00600806D9B6}']
    function Value: OleVariant; dispid 0;
    property Name: WideString readonly dispid 1;
    property IsLocal: WordBool readonly dispid 2;
    property PropagatesToSubclass: WordBool dispid 3;
    property PropagatesToInstance: WordBool dispid 4;
    property IsOverridable: WordBool dispid 5;
    property IsAmended: WordBool readonly dispid 6;
  end;

// *********************************************************************//
// Interface: ISWbemPropertySet
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {DEA0A7B2-D4BA-11D1-8B09-00600806D9B6}
// *********************************************************************//
  ISWbemPropertySet = interface(IDispatch)
    ['{DEA0A7B2-D4BA-11D1-8B09-00600806D9B6}']
    function Get__NewEnum: IUnknown; safecall;
    function Item(const strName: WideString; iFlags: Integer): ISWbemProperty; safecall;
    function Get_Count: Integer; safecall;
    function Add(const strName: WideString; iCimType: WbemCimtypeEnum; bIsArray: WordBool; 
                 iFlags: Integer): ISWbemProperty; safecall;
    procedure Remove(const strName: WideString; iFlags: Integer); safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  ISWbemPropertySetDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {DEA0A7B2-D4BA-11D1-8B09-00600806D9B6}
// *********************************************************************//
  ISWbemPropertySetDisp = dispinterface
    ['{DEA0A7B2-D4BA-11D1-8B09-00600806D9B6}']
    property _NewEnum: IUnknown readonly dispid -4;
    function Item(const strName: WideString; iFlags: Integer): ISWbemProperty; dispid 0;
    property Count: Integer readonly dispid 1;
    function Add(const strName: WideString; iCimType: WbemCimtypeEnum; bIsArray: WordBool; 
                 iFlags: Integer): ISWbemProperty; dispid 2;
    procedure Remove(const strName: WideString; iFlags: Integer); dispid 3;
  end;

// *********************************************************************//
// Interface: ISWbemProperty
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {1A388F98-D4BA-11D1-8B09-00600806D9B6}
// *********************************************************************//
  ISWbemProperty = interface(IDispatch)
    ['{1A388F98-D4BA-11D1-8B09-00600806D9B6}']
    function Get_Value: OleVariant; safecall;
    procedure Set_Value(var varValue: OleVariant); safecall;
    function Get_Name: WideString; safecall;
    function Get_IsLocal: WordBool; safecall;
    function Get_Origin: WideString; safecall;
    function Get_CIMType: WbemCimtypeEnum; safecall;
    function Get_Qualifiers_: ISWbemQualifierSet; safecall;
    function Get_IsArray: WordBool; safecall;
    property Name: WideString read Get_Name;
    property IsLocal: WordBool read Get_IsLocal;
    property Origin: WideString read Get_Origin;
    property CIMType: WbemCimtypeEnum read Get_CIMType;
    property Qualifiers_: ISWbemQualifierSet read Get_Qualifiers_;
    property IsArray: WordBool read Get_IsArray;
  end;

// *********************************************************************//
// DispIntf:  ISWbemPropertyDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {1A388F98-D4BA-11D1-8B09-00600806D9B6}
// *********************************************************************//
  ISWbemPropertyDisp = dispinterface
    ['{1A388F98-D4BA-11D1-8B09-00600806D9B6}']
    function Value: OleVariant; dispid 0;
    property Name: WideString readonly dispid 1;
    property IsLocal: WordBool readonly dispid 2;
    property Origin: WideString readonly dispid 3;
    property CIMType: WbemCimtypeEnum readonly dispid 4;
    property Qualifiers_: ISWbemQualifierSet readonly dispid 5;
    property IsArray: WordBool readonly dispid 6;
  end;

// *********************************************************************//
// Interface: ISWbemMethodSet
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {C93BA292-D955-11D1-8B09-00600806D9B6}
// *********************************************************************//
  ISWbemMethodSet = interface(IDispatch)
    ['{C93BA292-D955-11D1-8B09-00600806D9B6}']
    function Get__NewEnum: IUnknown; safecall;
    function Item(const strName: WideString; iFlags: Integer): ISWbemMethod; safecall;
    function Get_Count: Integer; safecall;
    property _NewEnum: IUnknown read Get__NewEnum;
    property Count: Integer read Get_Count;
  end;

// *********************************************************************//
// DispIntf:  ISWbemMethodSetDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {C93BA292-D955-11D1-8B09-00600806D9B6}
// *********************************************************************//
  ISWbemMethodSetDisp = dispinterface
    ['{C93BA292-D955-11D1-8B09-00600806D9B6}']
    property _NewEnum: IUnknown readonly dispid -4;
    function Item(const strName: WideString; iFlags: Integer): ISWbemMethod; dispid 0;
    property Count: Integer readonly dispid 1;
  end;

// *********************************************************************//
// Interface: ISWbemMethod
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {422E8E90-D955-11D1-8B09-00600806D9B6}
// *********************************************************************//
  ISWbemMethod = interface(IDispatch)
    ['{422E8E90-D955-11D1-8B09-00600806D9B6}']
    function Get_Name: WideString; safecall;
    function Get_Origin: WideString; safecall;
    function Get_InParameters: ISWbemObject; safecall;
    function Get_OutParameters: ISWbemObject; safecall;
    function Get_Qualifiers_: ISWbemQualifierSet; safecall;
    property Name: WideString read Get_Name;
    property Origin: WideString read Get_Origin;
    property InParameters: ISWbemObject read Get_InParameters;
    property OutParameters: ISWbemObject read Get_OutParameters;
    property Qualifiers_: ISWbemQualifierSet read Get_Qualifiers_;
  end;

// *********************************************************************//
// DispIntf:  ISWbemMethodDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {422E8E90-D955-11D1-8B09-00600806D9B6}
// *********************************************************************//
  ISWbemMethodDisp = dispinterface
    ['{422E8E90-D955-11D1-8B09-00600806D9B6}']
    property Name: WideString readonly dispid 1;
    property Origin: WideString readonly dispid 2;
    property InParameters: ISWbemObject readonly dispid 3;
    property OutParameters: ISWbemObject readonly dispid 4;
    property Qualifiers_: ISWbemQualifierSet readonly dispid 5;
  end;

// *********************************************************************//
// Interface: ISWbemServices
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {76A6415C-CB41-11D1-8B02-00600806D9B6}
// *********************************************************************//
  ISWbemServices = interface(IDispatch)
    ['{76A6415C-CB41-11D1-8B02-00600806D9B6}']
    function Get(const strObjectPath: WideString; iFlags: Integer; 
                 const objWbemNamedValueSet: IDispatch): ISWbemObject; safecall;
    procedure GetAsync(const objWbemSink: IDispatch; const strObjectPath: WideString; 
                       iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                       const objWbemAsyncContext: IDispatch); safecall;
    procedure Delete(const strObjectPath: WideString; iFlags: Integer; 
                     const objWbemNamedValueSet: IDispatch); safecall;
    procedure DeleteAsync(const objWbemSink: IDispatch; const strObjectPath: WideString; 
                          iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                          const objWbemAsyncContext: IDispatch); safecall;
    function InstancesOf(const strClass: WideString; iFlags: Integer; 
                         const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; safecall;
    procedure InstancesOfAsync(const objWbemSink: IDispatch; const strClass: WideString; 
                               iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                               const objWbemAsyncContext: IDispatch); safecall;
    function SubclassesOf(const strSuperclass: WideString; iFlags: Integer; 
                          const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; safecall;
    procedure SubclassesOfAsync(const objWbemSink: IDispatch; const strSuperclass: WideString; 
                                iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                                const objWbemAsyncContext: IDispatch); safecall;
    function ExecQuery(const strQuery: WideString; const strQueryLanguage: WideString; 
                       iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; safecall;
    procedure ExecQueryAsync(const objWbemSink: IDispatch; const strQuery: WideString; 
                             const strQueryLanguage: WideString; lFlags: Integer; 
                             const objWbemNamedValueSet: IDispatch; 
                             const objWbemAsyncContext: IDispatch); safecall;
    function AssociatorsOf(const strObjectPath: WideString; const strAssocClass: WideString; 
                           const strResultClass: WideString; const strResultRole: WideString; 
                           const strRole: WideString; bClassesOnly: WordBool; 
                           bSchemaOnly: WordBool; const strRequiredAssocQualifier: WideString; 
                           const strRequiredQualifier: WideString; iFlags: Integer; 
                           const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; safecall;
    procedure AssociatorsOfAsync(const objWbemSink: IDispatch; const strObjectPath: WideString; 
                                 const strAssocClass: WideString; const strResultClass: WideString; 
                                 const strResultRole: WideString; const strRole: WideString; 
                                 bClassesOnly: WordBool; bSchemaOnly: WordBool; 
                                 const strRequiredAssocQualifier: WideString; 
                                 const strRequiredQualifier: WideString; iFlags: Integer; 
                                 const objWbemNamedValueSet: IDispatch; 
                                 const objWbemAsyncContext: IDispatch); safecall;
    function ReferencesTo(const strObjectPath: WideString; const strResultClass: WideString; 
                          const strRole: WideString; bClassesOnly: WordBool; bSchemaOnly: WordBool; 
                          const strRequiredQualifier: WideString; iFlags: Integer; 
                          const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; safecall;
    procedure ReferencesToAsync(const objWbemSink: IDispatch; const strObjectPath: WideString; 
                                const strResultClass: WideString; const strRole: WideString; 
                                bClassesOnly: WordBool; bSchemaOnly: WordBool; 
                                const strRequiredQualifier: WideString; iFlags: Integer; 
                                const objWbemNamedValueSet: IDispatch; 
                                const objWbemAsyncContext: IDispatch); safecall;
    function ExecNotificationQuery(const strQuery: WideString; const strQueryLanguage: WideString; 
                                   iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemEventSource; safecall;
    procedure ExecNotificationQueryAsync(const objWbemSink: IDispatch; const strQuery: WideString; 
                                         const strQueryLanguage: WideString; iFlags: Integer; 
                                         const objWbemNamedValueSet: IDispatch; 
                                         const objWbemAsyncContext: IDispatch); safecall;
    function ExecMethod(const strObjectPath: WideString; const strMethodName: WideString; 
                        const objWbemInParameters: IDispatch; iFlags: Integer; 
                        const objWbemNamedValueSet: IDispatch): ISWbemObject; safecall;
    procedure ExecMethodAsync(const objWbemSink: IDispatch; const strObjectPath: WideString; 
                              const strMethodName: WideString; 
                              const objWbemInParameters: IDispatch; iFlags: Integer; 
                              const objWbemNamedValueSet: IDispatch; 
                              const objWbemAsyncContext: IDispatch); safecall;
    function Get_Security_: ISWbemSecurity; safecall;
    property Security_: ISWbemSecurity read Get_Security_;
  end;

// *********************************************************************//
// DispIntf:  ISWbemServicesDisp
// Flags:     (4432) Hidden Dual OleAutomation Dispatchable
// GUID:      {76A6415C-CB41-11D1-8B02-00600806D9B6}
// *********************************************************************//
  ISWbemServicesDisp = dispinterface
    ['{76A6415C-CB41-11D1-8B02-00600806D9B6}']
    function Get(const strObjectPath: WideString; iFlags: Integer; 
                 const objWbemNamedValueSet: IDispatch): ISWbemObject; dispid 1;
    procedure GetAsync(const objWbemSink: IDispatch; const strObjectPath: WideString; 
                       iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                       const objWbemAsyncContext: IDispatch); dispid 2;
    procedure Delete(const strObjectPath: WideString; iFlags: Integer; 
                     const objWbemNamedValueSet: IDispatch); dispid 3;
    procedure DeleteAsync(const objWbemSink: IDispatch; const strObjectPath: WideString; 
                          iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                          const objWbemAsyncContext: IDispatch); dispid 4;
    function InstancesOf(const strClass: WideString; iFlags: Integer; 
                         const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 5;
    procedure InstancesOfAsync(const objWbemSink: IDispatch; const strClass: WideString; 
                               iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                               const objWbemAsyncContext: IDispatch); dispid 6;
    function SubclassesOf(const strSuperclass: WideString; iFlags: Integer; 
                          const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 7;
    procedure SubclassesOfAsync(const objWbemSink: IDispatch; const strSuperclass: WideString; 
                                iFlags: Integer; const objWbemNamedValueSet: IDispatch; 
                                const objWbemAsyncContext: IDispatch); dispid 8;
    function ExecQuery(const strQuery: WideString; const strQueryLanguage: WideString; 
                       iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 9;
    procedure ExecQueryAsync(const objWbemSink: IDispatch; const strQuery: WideString; 
                             const strQueryLanguage: WideString; lFlags: Integer; 
                             const objWbemNamedValueSet: IDispatch; 
                             const objWbemAsyncContext: IDispatch); dispid 10;
    function AssociatorsOf(const strObjectPath: WideString; const strAssocClass: WideString; 
                           const strResultClass: WideString; const strResultRole: WideString; 
                           const strRole: WideString; bClassesOnly: WordBool; 
                           bSchemaOnly: WordBool; const strRequiredAssocQualifier: WideString; 
                           const strRequiredQualifier: WideString; iFlags: Integer; 
                           const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 11;
    procedure AssociatorsOfAsync(const objWbemSink: IDispatch; const strObjectPath: WideString; 
                                 const strAssocClass: WideString; const strResultClass: WideString; 
                                 const strResultRole: WideString; const strRole: WideString; 
                                 bClassesOnly: WordBool; bSchemaOnly: WordBool; 
                                 const strRequiredAssocQualifier: WideString; 
                                 const strRequiredQualifier: WideString; iFlags: Integer; 
                                 const objWbemNamedValueSet: IDispatch; 
                                 const objWbemAsyncContext: IDispatch); dispid 12;
    function ReferencesTo(const strObjectPath: WideString; const strResultClass: WideString; 
                          const strRole: WideString; bClassesOnly: WordBool; bSchemaOnly: WordBool; 
                          const strRequiredQualifier: WideString; iFlags: Integer; 
                          const objWbemNamedValueSet: IDispatch): ISWbemObjectSet; dispid 13;
    procedure ReferencesToAsync(const objWbemSink: IDispatch; const strObjectPath: WideString; 
                                const strResultClass: WideString; const strRole: WideString; 
                                bClassesOnly: WordBool; bSchemaOnly: WordBool; 
                                const strRequiredQualifier: WideString; iFlags: Integer; 
                                const objWbemNamedValueSet: IDispatch; 
                                const objWbemAsyncContext: IDispatch); dispid 14;
    function ExecNotificationQuery(const strQuery: WideString; const strQueryLanguage: WideString; 
                                   iFlags: Integer; const objWbemNamedValueSet: IDispatch): ISWbemEventSource; dispid 15;
    procedure ExecNotificationQueryAsync(const objWbemSink: IDispatch; const strQuery: WideString; 
                                         const strQueryLanguage: WideString; iFlags: Integer; 
                                         const objWbemNamedValueSet: IDispatch; 
                                         const objWbemAsyncContext: IDispatch); dispid 16;
    function ExecMethod(const strObjectPath: WideString; const strMethodName: WideString; 
                        const objWbemInParameters: IDispatch; iFlags: Integer; 
                        const objWbemNamedValueSet: IDispatch): ISWbemObject; dispid 17;
    procedure ExecMethodAsync(const objWbemSink: IDispatch; const strObjectPath: WideString; 
                              const strMethodName: WideString; 
                              const objWbemInParameters: IDispatch; iFlags: Integer; 
                              const objWbemNamedValueSet: IDispatch; 
                              const objWbemAsyncContext: IDispatch); dispid 18;
    property Security_: ISWbemSecurity readonly dispid 19;
  end;

// *********************************************************************//
// Interface: ISWbemEventSource
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {27D54D92-0EBE-11D2-8B22-00600806D9B6}
// *********************************************************************//
  ISWbemEventSource = interface(IDispatch)
    ['{27D54D92-0EBE-11D2-8B22-00600806D9B6}']
    function NextEvent(iTimeoutMs: Integer): ISWbemObject; safecall;
    function Get_Security_: ISWbemSecurity; safecall;
    property Security_: ISWbemSecurity read Get_Security_;
  end;

// *********************************************************************//
// DispIntf:  ISWbemEventSourceDisp
// Flags:     (4560) Hidden Dual NonExtensible OleAutomation Dispatchable
// GUID:      {27D54D92-0EBE-11D2-8B22-00600806D9B6}
// *********************************************************************//
  ISWbemEventSourceDisp = dispinterface
    ['{27D54D92-0EBE-11D2-8B22-00600806D9B6}']
    function NextEvent(iTimeoutMs: Integer): ISWbemObject; dispid 1;
    property Security_: ISWbemSecurity readonly dispid 2;
  end;

// *********************************************************************//
// The Class CoWMIExtension provides a Create and CreateRemote method to          
// create instances of the default interface IWMIExtension exposed by              
// the CoClass WMIExtension. The functions are intended to be used by             
// clients wishing to automate the CoClass objects exposed by the         
// server of this typelibrary.                                            
// *********************************************************************//
  CoWMIExtension = class
    class function Create: IWMIExtension;
    class function CreateRemote(const MachineName: string): IWMIExtension;
  end;


// *********************************************************************//
// OLE Server Proxy class declaration
// Server Object    : TWMIExtension
// Help String      : WMI DS Extension class
// Default Interface: IWMIExtension
// Def. Intf. DISP? : No
// Event   Interface: 
// TypeFlags        : (2) CanCreate
// *********************************************************************//
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  TWMIExtensionProperties= class;
{$ENDIF}
  TWMIExtension = class(TOleServer)
  private
    FIntf: IWMIExtension;
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    FProps: TWMIExtensionProperties;
    function GetServerProperties: TWMIExtensionProperties;
{$ENDIF}
    function GetDefaultInterface: IWMIExtension;
  protected
    procedure InitServerData; override;
    function Get_WMIObjectPath: WideString;
  public
    constructor Create(AOwner: TComponent); override;
    destructor  Destroy; override;
    procedure Connect; override;
    procedure ConnectTo(svrIntf: IWMIExtension);
    procedure Disconnect; override;
    function GetWMIObject: ISWbemObject;
    function GetWMIServices: ISWbemServices;
    property DefaultInterface: IWMIExtension read GetDefaultInterface;
    property WMIObjectPath: WideString read Get_WMIObjectPath;
  published
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
    property Server: TWMIExtensionProperties read GetServerProperties;
{$ENDIF}
  end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
// *********************************************************************//
// OLE Server Properties Proxy Class
// Server Object    : TWMIExtension
// (This object is used by the IDE's Property Inspector to allow editing
//  of the properties of this server)
// *********************************************************************//
 TWMIExtensionProperties = class(TPersistent)
  private
    FServer:    TWMIExtension;
    function    GetDefaultInterface: IWMIExtension;
    constructor Create(AServer: TWMIExtension);
  protected
    function Get_WMIObjectPath: WideString;
  public
    property DefaultInterface: IWMIExtension read GetDefaultInterface;
  published
  end;
{$ENDIF}


procedure Register;

resourcestring
  dtlServerPage = 'WMI';

  dtlOcxPage = 'WMI';

implementation

uses ComObj;

class function CoWMIExtension.Create: IWMIExtension;
begin
  Result := CreateComObject(CLASS_WMIExtension) as IWMIExtension;
end;

class function CoWMIExtension.CreateRemote(const MachineName: string): IWMIExtension;
begin
  Result := CreateRemoteComObject(MachineName, CLASS_WMIExtension) as IWMIExtension;
end;

procedure TWMIExtension.InitServerData;
const
  CServerData: TServerData = (
    ClassID:   '{F0975AFE-5C7F-11D2-8B74-00104B2AFB41}';
    IntfIID:   '{ADC1F06E-5C7E-11D2-8B74-00104B2AFB41}';
    EventIID:  '';
    LicenseKey: nil;
    Version: 500);
begin
  ServerData := @CServerData;
end;

procedure TWMIExtension.Connect;
var
  punk: IUnknown;
begin
  if FIntf = nil then
  begin
    punk := GetServer;
    Fintf:= punk as IWMIExtension;
  end;
end;

procedure TWMIExtension.ConnectTo(svrIntf: IWMIExtension);
begin
  Disconnect;
  FIntf := svrIntf;
end;

procedure TWMIExtension.DisConnect;
begin
  if Fintf <> nil then
  begin
    FIntf := nil;
  end;
end;

function TWMIExtension.GetDefaultInterface: IWMIExtension;
begin
  if FIntf = nil then
    Connect;
  Assert(FIntf <> nil, 'DefaultInterface is NULL. Component is not connected to Server. You must call "Connect" or "ConnectTo" before this operation');
  Result := FIntf;
end;

constructor TWMIExtension.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps := TWMIExtensionProperties.Create(Self);
{$ENDIF}
end;

destructor TWMIExtension.Destroy;
begin
{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
  FProps.Free;
{$ENDIF}
  inherited Destroy;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
function TWMIExtension.GetServerProperties: TWMIExtensionProperties;
begin
  Result := FProps;
end;
{$ENDIF}

function TWMIExtension.Get_WMIObjectPath: WideString;
begin
    Result := DefaultInterface.WMIObjectPath;
end;

function TWMIExtension.GetWMIObject: ISWbemObject;
begin
  Result := DefaultInterface.GetWMIObject;
end;

function TWMIExtension.GetWMIServices: ISWbemServices;
begin
  Result := DefaultInterface.GetWMIServices;
end;

{$IFDEF LIVE_SERVER_AT_DESIGN_TIME}
constructor TWMIExtensionProperties.Create(AServer: TWMIExtension);
begin
  inherited Create;
  FServer := AServer;
end;

function TWMIExtensionProperties.GetDefaultInterface: IWMIExtension;
begin
  Result := FServer.DefaultInterface;
end;

function TWMIExtensionProperties.Get_WMIObjectPath: WideString;
begin
    Result := DefaultInterface.WMIObjectPath;
end;

{$ENDIF}

procedure Register;
begin
  RegisterComponents(dtlServerPage, [TWMIExtension]);
end;

end.
