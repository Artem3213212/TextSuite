unit uglcContextGtkCustomVisual;

{ Package:      OpenGLCore
  Prefix:       glc - OpenGL Core
  Beschreibung: diese Unit enthält Klassen zum Erzeugen von Visuals (unter Linux),
                auf denen ein OpenGL Kontext erstellt werden kann }

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, LCLType, InterfaceBase, LMessages, WSLCLClasses, WSControls,
  X, XLib, glib2, gdk2, gdk2x, gtk2, Gtk2Def, Gtk2Int;

type
  TCustomVisualControl = class(TWinControl)
  private
    FIntWidget: PGtkWidget;
    FVisualID: TVisualID;
  protected
    function WSCreateHandle({%H-}const WSPrivate: TWSPrivateClass; const AParams: TCreateParams): TLCLIntfHandle;
    procedure WSBeforeDestroyHandle;
  public
    constructor Create(TheOwner: TComponent; const aVisualID: TVisualID); overload;
    property Widget: PGtkWidget read FIntWidget;
  end;


  TWSCustomVisualControl = class(TWSWinControl)
  published
    class function CreateHandle(const AWinControl: TWinControl;
                                const AParams: TCreateParams): TLCLIntfHandle; override;
    class procedure DestroyHandle(const AWinControl: TWinControl); override;
  end;


implementation

type
  PGtkCustomWidget = ^TGtkCustomWidget;
  TGtkCustomWidget = record
    darea: TGtkDrawingArea;
  end;

  PGtkCustomWidgetClass = ^TGtkCustomWidgetClass;
  TGtkCustomWidgetClass = record
    parent_class: TGtkDrawingAreaClass;
  end;

var
  custom_widget_type: TGtkType = 0;
  custom_widget_parent_class: Pointer = nil;

function GTK_TYPE_CUSTOM_WIDGET: TGtkType; forward;


procedure g_return_if_fail(b: boolean; const Msg: string);
begin
  if not b then raise Exception.Create(Msg);
end;

procedure g_return_if_fail(b: boolean);
begin
  g_return_if_fail(b,'');
end;

function GTK_IS_CUSTOM_WIDGET(obj: Pointer): Boolean;
begin
  GTK_IS_CUSTOM_WIDGET:=GTK_CHECK_TYPE(obj,GTK_TYPE_CUSTOM_WIDGET);
end;

function GTK_CUSTOM_WIDGET(obj: Pointer): PGtkCustomWidget;
begin
  g_return_if_fail(GTK_IS_CUSTOM_WIDGET(obj),'');
  Result:=PGtkCustomWidget(obj);
end;

procedure gtk_custom_widget_init(custom_widget: PGTypeInstance; theClass: gpointer); cdecl;
begin
  if theClass=nil then ;
  //DebugLn(['gtk_custom_widget_init START']);
  gtk_widget_set_double_buffered(PGtkWidget(custom_widget),gdkFALSE);
  GTK_WIDGET_UNSET_FLAGS(PGtkWidget(custom_widget),GTK_NO_WINDOW);
  //DebugLn(['gtk_custom_widget_init END']);
end;

procedure gtk_custom_widget_destroy(obj: PGtkObject); cdecl;
begin
  g_return_if_fail (obj <>nil,'');
  g_return_if_fail (GTK_IS_CUSTOM_WIDGET(obj),'');

  if Assigned(GTK_OBJECT_CLASS(custom_widget_parent_class)^.destroy) then
    GTK_OBJECT_CLASS(custom_widget_parent_class)^.destroy(obj);
end;

procedure gtk_custom_widget_class_init(klass: Pointer); cdecl;
var
  object_class: PGtkObjectClass;
begin
  custom_widget_parent_class := gtk_type_class(gtk_drawing_area_get_type());
  g_return_if_fail(custom_widget_parent_class<>nil,'gtk_custom_widget_class_init parent_class=nil');
  object_class := PGtkObjectClass(klass);
  g_return_if_fail(object_class<>nil,'gtk_custom_widget_class_init object_class=nil');

  object_class^.destroy := @gtk_custom_widget_destroy;
end;

function custom_widget_size_allocateCB(Widget: PGtkWidget; Size: pGtkAllocation;
  Data: gPointer): GBoolean; cdecl;
const
  CallBackDefaultReturn = {$IFDEF GTK2}false{$ELSE}true{$ENDIF};
var
  SizeMsg: TLMSize;
  GtkWidth, GtkHeight: integer;
  LCLControl: TWinControl;
begin
  Result := CallBackDefaultReturn;
  if not GTK_WIDGET_REALIZED(Widget) then begin
    // the widget is not yet realized, so this GTK resize was not a user change.
    // => ignore
    exit;
  end;
  if Size=nil then ;
  LCLControl:=TWinControl(Data);
  if LCLControl=nil then exit;
  //DebugLn(['gtkglarea_size_allocateCB ',DbgSName(LCLControl)]);

  gtk_widget_get_size_request(Widget, @GtkWidth, @GtkHeight);

  SizeMsg.Msg:=0;
  FillChar(SizeMsg,SizeOf(SizeMsg),0);
  with SizeMsg do
  begin
    Result := 0;
    Msg := LM_SIZE;
    SizeType := Size_SourceIsInterface;
    Width := SmallInt(GtkWidth);
    Height := SmallInt(GtkHeight);
  end;
  //DebugLn(['gtkglarea_size_allocateCB ',GtkWidth,',',GtkHeight]);
  LCLControl.WindowProc(TLMessage(SizeMsg));
end;

function GTK_TYPE_CUSTOM_WIDGET: TGtkType;
const
  custom_widget_type_name = 'GtkGLArea';
  custom_widget_info: TGtkTypeInfo = (
    type_name: custom_widget_type_name;
    object_size: SizeOf(TGtkCustomWidget);
    class_size:  SizeOf(TGtkCustomWidgetClass);
    class_init_func:  @gtk_custom_widget_class_init;
    object_init_func: @gtk_custom_widget_init;
    reserved_1: nil;
    reserved_2: nil;
    base_class_init_func: nil;
  );
begin
  if (custom_widget_type=0) then begin
    custom_widget_type:=gtk_type_unique(gtk_drawing_area_get_type(),@custom_widget_info);
  end;
  Result:=custom_widget_type;
end;

{ TCustomVisualControl }

constructor TCustomVisualControl.Create(TheOwner: TComponent; const aVisualID: TVisualID);
begin
  inherited Create(TheOwner);
  FIntWidget:= nil;
  fVisualID:= aVisualID;
  SetBounds(0, 0, 200, 200);
end;

function TCustomVisualControl.WSCreateHandle(const WSPrivate: TWSPrivateClass; const AParams: TCreateParams): TLCLIntfHandle;
var
  cmap: PGdkColormap;
  gdkvis: PGdkVisual;
begin
  // is the requested VisualID different from what the widget would get?
  cmap  := gdk_colormap_get_system;
  gdkvis:= gdk_colormap_get_visual(cmap);
  if XVisualIDFromVisual(gdk_x11_visual_get_xvisual(gdkvis)) <> FVisualID then begin
    gdkvis:= gdkx_visual_get(FVisualID);
    cmap  := gdk_colormap_new(gdkvis, false);
  end;

  FIntWidget:= gtk_type_new(GTK_TYPE_CUSTOM_WIDGET);
  gtk_widget_set_colormap(FIntWidget, cmap);

  Result:= TLCLIntfHandle({%H-}PtrUInt(FIntWidget));
  PGtkobject(FIntWidget)^.flags:= PGtkobject(FIntWidget)^.flags or GTK_CAN_FOCUS;
  TGTK2WidgetSet(WidgetSet).FinishCreateHandle(Self,FIntWidget,AParams);
  g_signal_connect_after(FIntWidget, 'size-allocate', TGTKSignalFunc(@custom_widget_size_allocateCB), Self);
end;

procedure TCustomVisualControl.WSBeforeDestroyHandle;
begin
  if not HandleAllocated then exit;
end;


{ TWSCustomVisualControl }

class function TWSCustomVisualControl.CreateHandle(const AWinControl: TWinControl; const AParams: TCreateParams): TLCLIntfHandle;
begin
  if csDesigning in AWinControl.ComponentState then begin
    // do not use "inherited CreateHandle", because the LCL changes the hierarchy at run time
    Result:= TWSWinControlClass(ClassParent).CreateHandle(AWinControl,AParams);
  end else
    Result:= (AWinControl as TCustomVisualControl).WSCreateHandle(WSPrivate, AParams);
end;

class procedure TWSCustomVisualControl.DestroyHandle(const AWinControl: TWinControl);
begin
  (AWinControl as TCustomVisualControl).WSBeforeDestroyHandle;
  // do not use "inherited DestroyHandle", because the LCL changes the hierarchy at run time
  TWSWinControlClass(ClassParent).DestroyHandle(AWinControl);
end;

initialization
  RegisterWSComponent(TCustomVisualControl,TWSCustomVisualControl);

end.

