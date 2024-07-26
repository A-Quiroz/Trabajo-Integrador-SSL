import ply.lex as lex
import ply.yacc as yacc
import tkinter as tk
from tkinter import filedialog
import sys

errores = 0

tokens = ('COMA','DOS_PUNTOS','LLAVE_IZQUIERDA','LLAVE_DERECHA','CORCHETE_IZQUIERDO','CORCHETE_DERECHO','VERSION', 'FIRMA_DIGITAL', 
'EMPRESAS', 'NOMBRE_EMPRESA','FUNDACION', 'INGRESOS_ANUALES', 'PYME', 'LINK','URL', 'DIRECCION', 'CALLE','PAIS', 
'CIUDAD', 'DEPARTAMENTOS', 'NOMBRE','JEFE','SUBDEPARTAMENTOS','EMPLEADOS',
'EDAD','CARGO','C_V','SALARIO','ACTIVO','FECHA_CONTRATACION','PROYECTOS','ESTADO','E_V','FECHA_INICIO',
'FECHA_FIN','NULL', 'ENTERO', 'FLOTANTE', 'BOOLEANO', 'FECHA','CADENA_DE_TEXTO')

t_ignore = ' \t'
t_COMA = r'\,'
t_DOS_PUNTOS = r'\:'
t_CORCHETE_IZQUIERDO = r'\['
t_CORCHETE_DERECHO = r'\]'
t_LLAVE_IZQUIERDA = r'\{'
t_LLAVE_DERECHA = r'\}'
t_URL = r'"https?://(?:[a-zA-Z0-9-_./#:]+)(?:/[a-zA-Z0-9-_./#:]+)*"'
t_ENTERO = r'\d+'
t_FLOTANTE = r'[+-]?(\d+\.\d\d)'

def t_EMPRESAS(t):
    r'"empresas"'
    return t 

def t_NOMBRE_EMPRESA(t):
    r'"nombre_empresa"'
    return t

def t_FUNDACION(t):
    r'"fundacion"'
    return t

def t_INGRESOS_ANUALES(t):
    r'"ingresos_anuales"'
    return t

def t_PYME(t):
    r'"pyme"'
    return t

def t_LINK(t): 
    r'"link"'
    return t

def t_DIRECCION(t):
    r'"direccion"'
    return t

def t_CALLE(t):
    r'"calle"'
    return t

def t_PAIS(t):
    r'"pais"'
    return t

def t_CIUDAD(t):
    r'"ciudad"'
    return t

def t_DEPARTAMENTOS(t): 
    r'"departamentos"'
    return t

def t_NOMBRE(t): 
    r'"nombre"'
    t.value = t.value[1:-1]
    return t

def t_JEFE(t): 
    r'"jefe"'
    return t 

def t_SUBDEPARTAMENTOS(t):
    r'"subdepartamentos"'
    return t

def t_EMPLEADOS(t):
    r'"empleados"'
    return t 

def t_EDAD(t):
    r'"edad"'
    return t

def t_CARGO(t):
    r'"cargo"'
    return t

def t_C_V(t):
    r'"(Product\sAnalyst|Project\sManager|UX\sdesigner|DB\sAdmin|Marketing|Developer|Devops)"'
    return t 

def t_SALARIO(t):
    r'"salario"'
    return t 

def t_ACTIVO(t):
    r'"activo"'
    return t

def t_FECHA_CONTRATACION(t):
    r'"fecha_contratacion"'
    return t

def t_PROYECTOS(t):
    r'"proyectos"'
    return t

def t_ESTADO(t):
    r'"estado"'
    t.value = t.value[1:-1]
    return t

def t_E_V(t):
    r'"(To\sdo|In\sprogress|Canceled|On\shold|Done)"'
    t.value = t.value[1:-1]
    return t 

def t_FECHA_INICIO(t):
    r'"fecha_inicio"'
    t.value = t.value[1:-1]
    return t

def t_FECHA_FIN(t):
    r'"fecha_fin"'
    t.value = t.value[1:-1]
    return t

def t_VERSION(t): 
    r'"version"'
    return t 

def t_FIRMA_DIGITAL(t):
    r'"firma_digital"'
    return t 

def t_BOOLEANO(t):
    r'true|false'
    return t 

def t_NULL(t):
    r'null'
    if t.value =='null':
        t.value = None
    return t

def t_FECHA(t):
    r'"(19[0-9][0-9]|20[0-9][0-9])\-(0?[1-9]|1[012])\-(0?[1-9]|[12][0-9]|3[012])"'
    t.value = t.value[1:-1]
    return t

def t_CADENA_DE_TEXTO(t):
    r'"[a-zA-Záéíóúñ0-9][a-zA-Záéíóúñ0-9\.\-\s]*"'
    t.value = t.value[1:-1]
    return t

def t_error(t):
    t.lexer.skip(1)

def t_SALTO_DE_LINEA(t):
    r'\n+'
    t.lexer.lineno += len(t.value)
    
def eliminar_saltos_de_linea(obj):
    if isinstance(obj, str):
        return obj.replace("\n", "")
    return obj

def p_sigma(p):
    '''sigma : LLAVE_IZQUIERDA EMPRESAS DOS_PUNTOS CORCHETE_IZQUIERDO emps CORCHETE_DERECHO LLAVE_DERECHA cierre_arch
        | LLAVE_IZQUIERDA EMPRESAS DOS_PUNTOS CORCHETE_IZQUIERDO emps CORCHETE_DERECHO COMA FIRMA_DIGITAL DOS_PUNTOS opc_s COMA VERSION DOS_PUNTOS opc_s LLAVE_DERECHA cierre_arch
        | LLAVE_IZQUIERDA EMPRESAS DOS_PUNTOS CORCHETE_IZQUIERDO emps CORCHETE_DERECHO COMA VERSION DOS_PUNTOS opc_s COMA FIRMA_DIGITAL DOS_PUNTOS opc_s LLAVE_DERECHA cierre_arch
        | LLAVE_IZQUIERDA EMPRESAS DOS_PUNTOS CORCHETE_IZQUIERDO emps CORCHETE_DERECHO COMA FIRMA_DIGITAL DOS_PUNTOS opc_s LLAVE_DERECHA cierre_arch
        | LLAVE_IZQUIERDA EMPRESAS DOS_PUNTOS CORCHETE_IZQUIERDO emps CORCHETE_DERECHO COMA VERSION DOS_PUNTOS opc_s LLAVE_DERECHA cierre_arch
        | LLAVE_IZQUIERDA FIRMA_DIGITAL DOS_PUNTOS opc_s COMA VERSION DOS_PUNTOS opc_s COMA EMPRESAS DOS_PUNTOS CORCHETE_IZQUIERDO emps CORCHETE_DERECHO LLAVE_DERECHA cierre_arch
        | LLAVE_IZQUIERDA FIRMA_DIGITAL DOS_PUNTOS opc_s COMA EMPRESAS DOS_PUNTOS CORCHETE_IZQUIERDO emps CORCHETE_DERECHO COMA VERSION DOS_PUNTOS opc_s LLAVE_DERECHA cierre_arch
        | LLAVE_IZQUIERDA FIRMA_DIGITAL DOS_PUNTOS opc_s COMA EMPRESAS DOS_PUNTOS CORCHETE_IZQUIERDO emps CORCHETE_DERECHO LLAVE_DERECHA cierre_arch
        | LLAVE_IZQUIERDA VERSION DOS_PUNTOS opc_s COMA FIRMA_DIGITAL DOS_PUNTOS opc_s COMA EMPRESAS DOS_PUNTOS CORCHETE_IZQUIERDO emps CORCHETE_DERECHO LLAVE_DERECHA cierre_arch '''
    
def p_emps(p):
    '''emps : LLAVE_IZQUIERDA emp LLAVE_DERECHA
        | LLAVE_IZQUIERDA emp LLAVE_DERECHA COMA emps'''
        
def p_emp(p):  
    '''emp : aper_emp NOMBRE_EMPRESA DOS_PUNTOS CADENA_DE_TEXTO nom_emp COMA FUNDACION DOS_PUNTOS ENTERO COMA DIRECCION DOS_PUNTOS LLAVE_IZQUIERDA diro LLAVE_DERECHA COMA INGRESOS_ANUALES DOS_PUNTOS E_F COMA PYME DOS_PUNTOS BOOLEANO COMA LINK DOS_PUNTOS url COMA DEPARTAMENTOS DOS_PUNTOS CORCHETE_IZQUIERDO dptos CORCHETE_DERECHO cierre_emp
        |  aper_emp NOMBRE_EMPRESA DOS_PUNTOS CADENA_DE_TEXTO nom_emp COMA FUNDACION DOS_PUNTOS ENTERO COMA DIRECCION DOS_PUNTOS LLAVE_IZQUIERDA diro LLAVE_DERECHA COMA INGRESOS_ANUALES DOS_PUNTOS E_F COMA PYME DOS_PUNTOS BOOLEANO COMA DEPARTAMENTOS DOS_PUNTOS CORCHETE_IZQUIERDO dptos CORCHETE_DERECHO cierre_emp
        |  aper_emp NOMBRE_EMPRESA DOS_PUNTOS CADENA_DE_TEXTO nom_emp COMA FUNDACION DOS_PUNTOS ENTERO COMA INGRESOS_ANUALES DOS_PUNTOS E_F COMA PYME DOS_PUNTOS BOOLEANO COMA LINK DOS_PUNTOS url COMA DEPARTAMENTOS DOS_PUNTOS CORCHETE_IZQUIERDO dptos CORCHETE_DERECHO cierre_emp
        |  aper_emp NOMBRE_EMPRESA DOS_PUNTOS CADENA_DE_TEXTO nom_emp COMA FUNDACION DOS_PUNTOS ENTERO COMA INGRESOS_ANUALES DOS_PUNTOS E_F COMA PYME DOS_PUNTOS BOOLEANO COMA DEPARTAMENTOS DOS_PUNTOS CORCHETE_IZQUIERDO dptos CORCHETE_DERECHO cierre_emp '''

def p_diro(p):
    '''diro : CALLE DOS_PUNTOS CADENA_DE_TEXTO COMA CIUDAD DOS_PUNTOS CADENA_DE_TEXTO COMA PAIS DOS_PUNTOS CADENA_DE_TEXTO
        | CALLE DOS_PUNTOS CADENA_DE_TEXTO COMA PAIS DOS_PUNTOS CADENA_DE_TEXTO COMA CIUDAD DOS_PUNTOS CADENA_DE_TEXTO
        | CIUDAD DOS_PUNTOS CADENA_DE_TEXTO COMA CALLE DOS_PUNTOS CADENA_DE_TEXTO COMA PAIS DOS_PUNTOS CADENA_DE_TEXTO
        | CIUDAD DOS_PUNTOS CADENA_DE_TEXTO COMA PAIS DOS_PUNTOS CADENA_DE_TEXTO COMA CALLE DOS_PUNTOS CADENA_DE_TEXTO
        | PAIS DOS_PUNTOS CADENA_DE_TEXTO COMA CIUDAD DOS_PUNTOS CADENA_DE_TEXTO COMA CALLE DOS_PUNTOS CADENA_DE_TEXTO
        | PAIS DOS_PUNTOS CADENA_DE_TEXTO COMA CALLE DOS_PUNTOS CADENA_DE_TEXTO COMA CIUDAD DOS_PUNTOS CADENA_DE_TEXTO'''
    
def p_dptos(p):
    '''dptos : LLAVE_IZQUIERDA dpto LLAVE_DERECHA
        | LLAVE_IZQUIERDA dpto LLAVE_DERECHA COMA dptos'''
    
def p_dpto(p):
    '''dpto : NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_depto COMA JEFE DOS_PUNTOS opc_s COMA SUBDEPARTAMENTOS DOS_PUNTOS CORCHETE_IZQUIERDO subs CORCHETE_DERECHO
        | NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_depto COMA SUBDEPARTAMENTOS DOS_PUNTOS CORCHETE_IZQUIERDO subs CORCHETE_DERECHO COMA JEFE DOS_PUNTOS opc_s
        | JEFE DOS_PUNTOS opc_s COMA NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_depto COMA SUBDEPARTAMENTOS DOS_PUNTOS CORCHETE_IZQUIERDO subs CORCHETE_DERECHO
        | JEFE DOS_PUNTOS opc_s COMA SUBDEPARTAMENTOS DOS_PUNTOS CORCHETE_IZQUIERDO subs CORCHETE_DERECHO COMA NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_depto
        | SUBDEPARTAMENTOS DOS_PUNTOS CORCHETE_IZQUIERDO subs CORCHETE_DERECHO COMA JEFE DOS_PUNTOS opc_s COMA NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_depto
        | SUBDEPARTAMENTOS DOS_PUNTOS CORCHETE_IZQUIERDO subs CORCHETE_DERECHO COMA NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_depto COMA JEFE DOS_PUNTOS opc_s
        | NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_depto COMA SUBDEPARTAMENTOS DOS_PUNTOS CORCHETE_IZQUIERDO subs CORCHETE_DERECHO
        | SUBDEPARTAMENTOS DOS_PUNTOS CORCHETE_IZQUIERDO subs CORCHETE_DERECHO COMA NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_depto '''

def p_subs(p):
    '''subs : LLAVE_IZQUIERDA sub LLAVE_DERECHA
        | LLAVE_IZQUIERDA sub LLAVE_DERECHA COMA subs'''
    
def p_sub(p):
    '''sub : NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_sub COMA JEFE DOS_PUNTOS opc_s COMA EMPLEADOS DOS_PUNTOS CORCHETE_IZQUIERDO tras CORCHETE_DERECHO
        | NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_sub COMA JEFE DOS_PUNTOS opc_s COMA EMPLEADOS DOS_PUNTOS CORCHETE_IZQUIERDO CORCHETE_DERECHO
        | NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_sub COMA JEFE DOS_PUNTOS opc_s COMA EMPLEADOS DOS_PUNTOS NULL
        | NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_sub COMA EMPLEADOS DOS_PUNTOS CORCHETE_IZQUIERDO tras CORCHETE_DERECHO COMA JEFE DOS_PUNTOS opc_s
        | NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_sub COMA EMPLEADOS DOS_PUNTOS CORCHETE_IZQUIERDO CORCHETE_DERECHO COMA JEFE DOS_PUNTOS opc_s
        | NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_sub COMA EMPLEADOS DOS_PUNTOS NULL COMA JEFE DOS_PUNTOS opc_s
        | JEFE DOS_PUNTOS opc_s COMA NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_sub COMA EMPLEADOS DOS_PUNTOS CORCHETE_IZQUIERDO tras CORCHETE_DERECHO
        | JEFE DOS_PUNTOS opc_s COMA NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_sub COMA EMPLEADOS DOS_PUNTOS CORCHETE_IZQUIERDO CORCHETE_DERECHO
        | JEFE DOS_PUNTOS opc_s COMA NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_sub COMA EMPLEADOS DOS_PUNTOS NULL
        | JEFE DOS_PUNTOS opc_s COMA NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_sub
        | EMPLEADOS DOS_PUNTOS CORCHETE_IZQUIERDO CORCHETE_DERECHO COMA NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_sub
        | EMPLEADOS DOS_PUNTOS CORCHETE_IZQUIERDO tras CORCHETE_DERECHO COMA NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_sub
        | EMPLEADOS DOS_PUNTOS NULL COMA NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_sub
        | NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_sub COMA EMPLEADOS DOS_PUNTOS CORCHETE_IZQUIERDO tras CORCHETE_DERECHO
        | NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_sub COMA EMPLEADOS DOS_PUNTOS CORCHETE_IZQUIERDO CORCHETE_DERECHO
        | NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_sub COMA EMPLEADOS DOS_PUNTOS NULL
        | NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_sub '''
    
def p_tras(p):
    '''tras : LLAVE_IZQUIERDA tra LLAVE_DERECHA
        | LLAVE_IZQUIERDA tra LLAVE_DERECHA COMA tras
        | NULL'''

def p_tra(p):
    '''tra : aper_ul NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_tra COMA EDAD DOS_PUNTOS opc_i COMA CARGO DOS_PUNTOS C_V COMA SALARIO DOS_PUNTOS E_F COMA ACTIVO DOS_PUNTOS BOOLEANO COMA FECHA_CONTRATACION DOS_PUNTOS FECHA COMA PROYECTOS DOS_PUNTOS CORCHETE_IZQUIERDO pros CORCHETE_DERECHO cierre_tabla cierre_ul
        | aper_ul NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_tra COMA EDAD DOS_PUNTOS opc_i COMA CARGO DOS_PUNTOS C_V COMA SALARIO DOS_PUNTOS E_F COMA ACTIVO DOS_PUNTOS BOOLEANO COMA FECHA_CONTRATACION DOS_PUNTOS FECHA COMA PROYECTOS DOS_PUNTOS CORCHETE_IZQUIERDO CORCHETE_DERECHO cierre_ul
        | aper_ul NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_tra COMA EDAD DOS_PUNTOS opc_i COMA CARGO DOS_PUNTOS C_V COMA SALARIO DOS_PUNTOS E_F COMA ACTIVO DOS_PUNTOS BOOLEANO COMA FECHA_CONTRATACION DOS_PUNTOS FECHA COMA PROYECTOS DOS_PUNTOS NULL cierre_ul
        | aper_ul NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_tra COMA EDAD DOS_PUNTOS opc_i COMA CARGO DOS_PUNTOS C_V COMA SALARIO DOS_PUNTOS E_F COMA ACTIVO DOS_PUNTOS BOOLEANO COMA FECHA_CONTRATACION DOS_PUNTOS FECHA cierre_ul
        | aper_ul NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_tra COMA CARGO DOS_PUNTOS C_V COMA SALARIO DOS_PUNTOS E_F COMA ACTIVO DOS_PUNTOS BOOLEANO COMA FECHA_CONTRATACION DOS_PUNTOS FECHA COMA PROYECTOS DOS_PUNTOS CORCHETE_IZQUIERDO pros CORCHETE_DERECHO cierre_tabla cierre_ul
        | aper_ul NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_tra COMA CARGO DOS_PUNTOS C_V COMA SALARIO DOS_PUNTOS E_F COMA ACTIVO DOS_PUNTOS BOOLEANO COMA FECHA_CONTRATACION DOS_PUNTOS FECHA COMA PROYECTOS DOS_PUNTOS CORCHETE_IZQUIERDO CORCHETE_DERECHO cierre_ul
        | aper_ul NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_tra COMA CARGO DOS_PUNTOS C_V COMA SALARIO DOS_PUNTOS E_F COMA ACTIVO DOS_PUNTOS BOOLEANO COMA FECHA_CONTRATACION DOS_PUNTOS FECHA COMA PROYECTOS DOS_PUNTOS NULL cierre_ul
        | aper_ul NOMBRE DOS_PUNTOS CADENA_DE_TEXTO nombre_tra COMA CARGO DOS_PUNTOS C_V COMA SALARIO DOS_PUNTOS E_F COMA ACTIVO DOS_PUNTOS BOOLEANO COMA FECHA_CONTRATACION DOS_PUNTOS FECHA cierre_ul'''

def p_pros(p):
    '''pros : LLAVE_IZQUIERDA pro LLAVE_DERECHA
        | LLAVE_IZQUIERDA pro LLAVE_DERECHA COMA pros
        | NULL'''

def p_pro(p):
    '''pro : NOMBRE DOS_PUNTOS CADENA_DE_TEXTO COMA ESTADO DOS_PUNTOS E_V COMA FECHA_INICIO DOS_PUNTOS FECHA COMA FECHA_FIN DOS_PUNTOS opc_d tabla_1
        | NOMBRE DOS_PUNTOS CADENA_DE_TEXTO COMA FECHA_INICIO DOS_PUNTOS FECHA COMA FECHA_FIN DOS_PUNTOS opc_d tabla_3
        | NOMBRE DOS_PUNTOS CADENA_DE_TEXTO COMA ESTADO DOS_PUNTOS E_V COMA FECHA_INICIO DOS_PUNTOS FECHA tabla_2
        | NOMBRE DOS_PUNTOS CADENA_DE_TEXTO COMA FECHA_INICIO DOS_PUNTOS FECHA tabla_4'''

def p_opc_s(p):
    '''opc_s : CADENA_DE_TEXTO 
        | NULL'''

def p_opc_i(p):
    '''opc_i : ENTERO 
        | NULL'''
        
def p_E_F(p):
    '''E_F : ENTERO
        | FLOTANTE'''

def p_aper_emp(x):
    '''aper_emp : '''
    html.write (f'\n\t\t<div style="background-color: #DFEC9F ; border-width: 1px; border-style:solid; padding: 20px">')

def p_nom_emp(p):
    '''nom_emp :'''
    html.write(f'\n\t\t\t<h1 style="color: #030303; font-family: Arial Black; font-size: 55px;"> {p[-1]} </h1>')
    
def p_cierre_emp(x):
    '''cierre_emp :'''
    html.write(f'\n\t\t</div>')

def p_nombre(p):
    '''nombre_depto :'''
    html.write(f'\n\t\t\t<h2 style ="font-family: Arial; font-size: 43px"> {p[-1]} </h2>')

def p_nombre_sub(p):
    '''nombre_sub :'''
    html.write(f'\n\t\t\t<h3 style ="font-family: monospace; font-size: 35px"> {p[-1]} </h3>')

def p_aper_ul(p):
    '''aper_ul :'''   
    html.write('\n\t\t\t<ul>')

def p_nombre_tra(p):
    '''nombre_tra :'''
    html.write(f'\n\t\t\t\t<li style ="font-family: georgia; font-size: 35px"> {p[-1]} </li>')
    
def p_cierre_ul(p):
    '''cierre_ul :'''
    html.write('\n\t\t\t</ul>')
    
def p_opc_d(p):
    '''opc_d : FECHA
        |   NULL'''
    global fecha_fin
    fecha_fin = p[1]
    
def p_tabla_1(p):
    '''tabla_1 :'''
    global itabla_1
    if not itabla_1:
        html.write(f'{i_table}<table border="1">{i_tr}<tr>{i_th}<th {style_th}>NOMBRE PROYECTO</th>{i_th}<th {style_th}>ESTADO</th>{i_th}<th {style_th}>FECHA INICIO</th>{i_th}<th {style_th}>FECHA FIN</th></tr>{i_tr}<tr>{i_td}<td {style_td}>{p[-13]}</td>{i_td}<td {style_td}>{p[-9]}</td>{i_td}<td {style_td}>{p[-5]}</td>{i_td}<td {style_td}>{fecha_fin}</td>{i_tr}</tr>')
        itabla_1 = True
    else: html.write(f'{i_tr}<tr>{i_td}<td {style_td}>{p[-13]}</td>{i_td}<td {style_td}>{p[-9]}</td>{i_td}<td {style_td}>{p[-5]}</td>{i_td}<td {style_td}>{fecha_fin}</td>{i_tr}</tr>')
    
def p_tabla_2(p):
    '''tabla_2 :'''
    global itabla_2
    if not itabla_2:
        html.write(f'{i_table}<table border="1">{i_tr}<tr>{i_th}<th {style_th}>NOMBRE PROYECTO</th>{i_th}<th {style_th}>ESTADO</th>{i_th}<th {style_th}>FECHA INICIO</th></tr>{i_tr}<tr>{i_td}<td {style_td}>{p[-9]}</td>{i_td}<td {style_td}>{p[-5]}</td>{i_td}<td {style_td}>{p[-1]}</td>{i_tr}</tr>')
        itabla_2 = True
    else: html.write(f'{i_tr}<tr>{i_td}<td {style_td}>{p[-9]}</td>{i_td}<td {style_td}>{p[-5]}</td>{i_td}<td {style_td}>{p[-1]}</td>{i_tr}</tr>')
        
def p_tabla_3(p):
    '''tabla_3 :'''
    global itabla_3
    if not itabla_3:
        html.write(f'{i_table}<table border="1">{i_tr}<tr>{i_th}<th {style_th}>NOMBRE PROYECTO</th>{i_th}<th {style_th}>FECHA INICIO</th>{i_th}<th {style_th}>FECHA FIN</th>{i_tr}</tr>{i_tr}<tr><td {style_td}>{p[-9]}</td><td {style_td}>{p[-5]}</td><td {style_td}>{p[-1]}</td>{i_tr}</tr>')
        itabla_3 = True
    else: html.write(f'{i_tr}<tr>{i_td}<td {style_td}>{p[-9]}</td>{i_td}<td {style_td}>{p[-5]}</td>{i_td}<td {style_td}>{p[-1]}</td>{i_tr}</tr>')

def p_tabla_4(p):
    '''tabla_4 :'''
    global itabla_4
    if not itabla_4:
        html.write(f'{i_table}<table border="1">{i_tr}<tr><th {style_th}>NOMBRE PROYECTO</th><th {style_th}>FECHA INICIO</th>{i_tr}/tr>')
    else: html.write(f'{i_tr}<tr>{i_td}<td {style_td}>{p[-5]}</td>{i_td}<td {style_td}>{p[-1]}</td>{i_tr}</tr>')
    
def p_url(p):
    '''url : URL
        | NULL '''
    if p[1] is not None:
        html.write(f'\n\t\t\t<p style="font-size: 30px;">Link: <a href= {p[1]}>{p[1]}</a></p>\n\t')

def p_cierre_table(p):
    '''cierre_tabla : '''
    if itabla_1 or itabla_2 or itabla_3 or itabla_4:
        html.write(f'{i_table}</table>')
        inic_variables()

def p_cierre_arch(p):
    '''cierre_arch : '''
    html.write('\n\t</body>\n</html>')
    
style_th = f'style="background-color: #C8D1CF"'
style_td = f'style="background-color: #FCFDFC; font-size: 28px"'
i_table = f'\n\t\t\t\t'
i_tr = f'\n\t\t\t\t\t'
i_th = i_td = f'\n\t\t\t\t\t\t'

def abrir_archivo_json():
    # Obtener la ruta del archivo JSON seleccionado
    ruta_archivo = filedialog.askopenfilename(
        title="Seleccione un archivo JSON",
        filetypes=[("Archivos JSON", "*.json")],
        initialdir="."  # Directorio actual por defecto
    )
    with open(ruta_archivo, "r") as archivo:
        global info
        info = archivo.read()
    global html
    html = open(ruta_archivo.replace(".json", ".html"), "w")
    html.write('<!doctype html>\n<html>\n\t<body>')
    ventana_bienvenida.destroy()

def ingresar_datos():
    ventana_bienvenida.destroy()
    global ventana_escribir
    ventana_escribir = tk.Tk()
    ventana_escribir.configure(bg="#DFEC9F")
    ventana_escribir.title("Texto en formato JSON")
    ancho_pantalla = ventana_escribir.winfo_screenwidth()
    alto_pantalla = ventana_escribir.winfo_screenheight()
    ventana_escribir.geometry(f"{ancho_pantalla}x{alto_pantalla}")
    
    global entrada_texto
    entrada_texto = tk.Text(ventana_escribir, width=90, height=40)
    entrada_texto.pack()

    boton_analizar = tk.Button(ventana_escribir, text="Analizar texto", command=analizar_texto)
    boton_analizar.pack()
    
    ventana_escribir.mainloop()


def analizar_texto():
    global info
    info = entrada_texto.get(1.0,tk.END)
    
    global html
    html = open('texto.html', 'w')
    html.write('<!doctype html>\n\t<html>\n\t\t<body>')
    ventana_escribir.destroy()
    
def p_error(p):
    global errores
    if p:
        ventana_error = tk.Tk()
        ventana_error.title('Error')
        mensaje_error = tk.Label(ventana_error, text=f'Error de sintaxis en línea {p.lineno}. Culpable: {p.value}.')
        mensaje_error.pack()
        ventana_error.mainloop()
        errores += 1
        sys.exit ()

def inic_variables():
    global itabla_1, itabla_2, itabla_3, itabla_4 
    itabla_1 = False
    itabla_2 = False
    itabla_3 = False
    itabla_4 = False
    
inic_variables()
ventana_bienvenida = tk.Tk()
ventana_bienvenida.title("Analizador léxico y sintáctico - Syntax Savants")
ventana_bienvenida.geometry("800x200")
ventana_bienvenida.configure(bg="#DFEC9F")
mensaje_bienvenida = tk.Label(ventana_bienvenida, text="¡Bienvenido!\n ¿Desea abrir un archivo JSON o ingresar datos por teclado?", font=("Arial", 14),bg="#DFEC9F")
mensaje_bienvenida.pack()
boton_archivo = tk.Button(ventana_bienvenida,text="Abrir archivo JSON", padx= 10, pady= 10, command=abrir_archivo_json)
boton_archivo.pack()
boton_teclado = tk.Button(ventana_bienvenida,text="Ingresar datos", padx= 20, pady= 10, command=ingresar_datos)
boton_teclado.pack()
ventana_bienvenida.mainloop()

LEXER = lex.lex()  #armamos el lexer
parser = yacc.yacc()
result = parser.parse(info, lexer = LEXER)
   
if errores == 0:
    ventana_exito = tk.Tk()
    ventana_exito.geometry("1000x200")
    ventana_exito.configure(bg="#DFEC9F")
    mensaje_exito = tk.Label(ventana_exito, text = "JSON SINTACTICAMENTE CORRECTO. PUEDE VISUALIZAR EL ARCHIVO HTML GENERADO", font=("Arial", 14), bg="#DFEC9F")
    mensaje_exito.pack()
    boton_cerrar = tk.Button(ventana_exito, text="Cerrar", command=ventana_exito.destroy)
    boton_cerrar.pack()
    ventana_exito.mainloop()