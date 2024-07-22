import ply.lex as lex
import ply.yacc as yacc 
import os 
import json
import html
from html import escape


# Lista de tokens
tokens = (
    'L_LLAVE', 'R_LLAVE', 'L_CORCHETE', 'R_CORCHETE', 'DOSPUNTOS', 'COMA', 'FLOAT', 'INTEGER',
    'BOOLEAN', 't_VACIOS', 'DATE',"t_empresas", 't_nombre_empresa', 't_fundacion', 't_direccion',
    't_calle', 't_nombre', 't_ciudad', 't_pais', 't_ingresos_anuales', 't_link', 't_departamentos',
    't_subdepartamentos', 't_jefe', 't_edad', 't_cargo', 't_salario', 't_pyme', 't_activo', 't_fecha_contratacion',
    't_estado', 't_fecha_inicio', 't_fecha_fin', 't_version', 't_firma_digital', 't_empleados',
    't_proyectos', 'POSIBLE_CARGO', 'POSIBLE_ESTADO', 'LinkURL', 'STRING',
)

# Expresiones regulares para tokens
t_L_LLAVE = r'\{'
t_R_LLAVE = r'\}'
t_L_CORCHETE = r'\['
t_R_CORCHETE = r'\]'
t_DOSPUNTOS = r':'
t_COMA = r','
t_FLOAT = r'-?\d+\.\d+'
t_INTEGER = r'-?\d+'
t_BOOLEAN = r'true|false'
t_DATE = r'"(\d{4}-\d{2}-\d{2})"'
t_t_nombre = r'"nombre"'
t_t_empresas = r'"empresas"'
t_t_nombre_empresa = r'"nombre_empresa"'
t_t_fundacion = r'"fundacion"|"fundación"|"fundaciÃn"'
t_t_direccion = r'"direccion"|"dirección"|"direcciÃn"'
t_t_calle = r'"calle"'
t_POSIBLE_CARGO = r'"(Product\sAnalyst|Project\sManager|UX\sdesigner|Marketing|Developer|Devops|DB\sadmin)"'
t_POSIBLE_ESTADO = r'"To\sdo"|"In\sprogress"|"Canceled"|"Done"|"On\shold"'
t_t_ciudad = r'"ciudad"'
t_t_pais = r'"pais"|"paÃs"|"país"'
t_t_ingresos_anuales = r'"ingresos_anuales"'
t_t_departamentos = r'"departamentos"'
t_t_subdepartamentos = r'"subdepartamentos"'
t_t_cargo = r'"cargo"'
t_t_salario = r'"salario"'
t_t_activo = r'"activo"'
t_t_fecha_contratacion = r'"fecha_contratacion"|"fecha_contratación"|"fecha_contrataciÃn'
t_t_estado = r'"estado"'
t_t_fecha_inicio = r'"fecha_inicio"'
t_t_fecha_fin = r'"fecha_fin"'
t_t_version = r'"versión" | "version"'
t_t_firma_digital = r'"firma_digital"'
t_t_empleados = r'"empleados"'
t_t_proyectos = r'"proyectos"'
t_LinkURL = r'"(https?:\/\/[a-zA-Z0-9\-.%]+(:[0-9]+)?(\/[a-zA-Z0-9\-.%#,:]+)*)"'

def t_t_VACIOS(t):
    r'\{\s*\}|\[\s*\]|null'
    if t.value == '{}':
        t.value = {}
    elif t.value == '[]':
        t.value = []
    elif t.value == 'null':
        t.value = None
    return t

def t_t_link(t):
    r'"link"'
    t.type = "t_link"
    return t

def t_t_edad(t):
    r'"edad"'
    t.type = "t_edad"
    return t

def t_t_jefe(t):
    r'"jefe"'
    t.type = "t_jefe"
    return t

def t_t_pyme(t):
    r'"pyme"'
    t.type = 't_pyme'
    return t
t_STRING = r'"[^"]*"'  # LO COLOCO AL FINAL PARA QUE SEA LO UTLIMO QUE COMPARE


# ME SIRVE PARA QUE MI LEXER NO RECONOZCA LOS ESPACIOS EN BLANCO 
# Y LOS SALTOS DE LINEA O TABULACIONES
t_ignore = ' \t\n'

# MMUESTRA EL ERROR LÉXICO Y EN DONDE SUCEDIO
# Muestra el error léxico y en donde sucedió
def t_error(t):
    global lexer
    line_count = 1
    column_count = 1
    for char in lexer.lexdata[:t.lexpos]:
        if char == '\n':
            line_count += 1
            column_count = 1
        else:
            column_count += 1
    message = f"Error en la línea {line_count}, columna {column_count}: Token Inválido D:'{t.value[0]}'"
    print(message)
    t.lexer.skip(1)  # Saltar al siguiente carácter después del error

# Lexer
lexer = lex.lex()

# --------------------------PARSER------------------------------------#

def p_sigma(p):
    '''sigma : L_LLAVE JSON R_LLAVE'''
    p[0] = p[1:]

def p_JSON(p):
    '''JSON : EMPRESAS
            | EMPRESAS COMA FIRVER
            | FIRVER COMA EMPRESAS'''


def p_FIRVER(p):
    '''FIRVER : VERSION COMA FIRMADIG
              | FIRMADIG COMA VERSION
              | VERSION
              | FIRMADIG'''
    

def p_EMPRESAS(p):
    '''EMPRESAS : t_empresas DOSPUNTOS L_CORCHETE EMPRESA R_CORCHETE'''
    

def p_EMPRESA(p):
    '''EMPRESA : L_LLAVE INFO R_LLAVE
               | L_LLAVE INFO R_LLAVE COMA EMPRESA'''
   
def p_INFO(p):
    '''INFO : NOMBRE_E COMA FUNDACION COMA DIRECCION COMA INGRESOS_AN COMA PYME COMA LINK COMA DEPARTAMENTOS
            | NOMBRE_E COMA FUNDACION COMA INGRESOS_AN COMA PYME COMA LINK COMA DEPARTAMENTOS
            | NOMBRE_E COMA FUNDACION COMA DIRECCION COMA INGRESOS_AN COMA PYME COMA DEPARTAMENTOS
            | NOMBRE_E COMA FUNDACION COMA INGRESOS_AN COMA PYME COMA DEPARTAMENTOS'''
    
def p_DIRECCION(p):
    '''DIRECCION : t_direccion DOSPUNTOS L_LLAVE INFODIR R_LLAVE
                 | t_direccion DOSPUNTOS VACIOS'''
    

def p_INFODIR(p):
    '''INFODIR : CALLE COMA CIUDAD COMA PAIS
               | CIUDAD COMA CALLE COMA PAIS
               | CIUDAD COMA PAIS COMA CALLE
               | PAIS COMA CALLE COMA CIUDAD
               | PAIS COMA CIUDAD COMA CALLE'''
   

def p_DEPARTAMENTOS(p):
    '''DEPARTAMENTOS : t_departamentos DOSPUNTOS L_CORCHETE DEP R_CORCHETE'''
    

def p_DEP(p):
    '''DEP : L_LLAVE INFODEP R_LLAVE
           | L_LLAVE INFODEP R_LLAVE COMA DEP'''
   

def p_INFODEP(p):
    '''INFODEP : NOMBRE COMA JEFE COMA SUBDEPARTAMENTO
               | NOMBRE COMA SUBDEPARTAMENTO COMA JEFE
               | NOMBRE COMA SUBDEPARTAMENTO
               | SUBDEPARTAMENTO COMA NOMBRE
               | JEFE COMA NOMBRE COMA SUBDEPARTAMENTO
               | JEFE COMA SUBDEPARTAMENTO COMA NOMBRE
               | SUBDEPARTAMENTO COMA JEFE COMA NOMBRE
               | SUBDEPARTAMENTO COMA NOMBRE COMA JEFE'''
    

def p_SUBDEPARTAMENTO(p):
    '''SUBDEPARTAMENTO : t_subdepartamentos DOSPUNTOS L_CORCHETE SUBD R_CORCHETE'''
    

def p_SUBD(p):
    '''SUBD : L_LLAVE INFOSUBD R_LLAVE
            | L_LLAVE INFOSUBD R_LLAVE COMA SUBD'''


def p_INFOSUBD(p):
    '''INFOSUBD : NOMBRE COMA JEFE COMA EMPLEADOS
                | NOMBRE COMA EMPLEADOS COMA JEFE
                | JEFE COMA NOMBRE COMA EMPLEADOS
                | JEFE COMA EMPLEADOS COMA NOMBRE
                | EMPLEADOS COMA NOMBRE COMA JEFE
                | EMPLEADOS COMA JEFE COMA NOMBRE
                | NOMBRE COMA JEFE
                | NOMBRE COMA EMPLEADOS
                | JEFE COMA NOMBRE
                | EMPLEADOS COMA NOMBRE
                | NOMBRE'''


def p_EMPLEADOS(p):
    '''EMPLEADOS : t_empleados DOSPUNTOS L_CORCHETE EMP R_CORCHETE 
                 | t_empleados DOSPUNTOS VACIOS'''


def p_EMP(p):
    '''EMP : L_LLAVE NOMBRE COMA EDAD COMA CARGO COMA SALARIO COMA ACTIVO COMA FECHA_CON COMA PROYECTOS R_LLAVE
           | L_LLAVE NOMBRE COMA EDAD COMA CARGO COMA SALARIO COMA ACTIVO COMA FECHA_CON COMA PROYECTOS R_LLAVE COMA EMP
           | L_LLAVE NOMBRE COMA CARGO COMA SALARIO COMA ACTIVO COMA FECHA_CON COMA PROYECTOS R_LLAVE
           | L_LLAVE NOMBRE COMA CARGO COMA SALARIO COMA ACTIVO COMA FECHA_CON COMA PROYECTOS R_LLAVE COMA EMP
           | L_LLAVE NOMBRE COMA CARGO COMA SALARIO COMA ACTIVO COMA FECHA_CON R_LLAVE
           | L_LLAVE NOMBRE COMA CARGO COMA SALARIO COMA ACTIVO COMA FECHA_CON R_LLAVE COMA EMP
           | L_LLAVE NOMBRE COMA EDAD COMA CARGO COMA SALARIO COMA ACTIVO COMA FECHA_CON R_LLAVE
           | L_LLAVE NOMBRE COMA EDAD COMA CARGO COMA SALARIO COMA ACTIVO COMA FECHA_CON R_LLAVE COMA EMP'''


def p_PROYECTOS(p):
    '''PROYECTOS : t_proyectos DOSPUNTOS L_CORCHETE PROY R_CORCHETE
                 | t_proyectos DOSPUNTOS VACIOS'''

def p_PROY(p):
    '''PROY : L_LLAVE NOMBRE COMA ESTADO COMA FECHA_IN COMA FECHA_FIN R_LLAVE
            | L_LLAVE NOMBRE COMA ESTADO COMA FECHA_IN COMA FECHA_FIN R_LLAVE COMA PROY
            | L_LLAVE NOMBRE COMA FECHA_IN COMA FECHA_FIN R_LLAVE
            | L_LLAVE NOMBRE COMA FECHA_IN COMA FECHA_FIN R_LLAVE COMA PROY
            | L_LLAVE NOMBRE COMA ESTADO COMA FECHA_IN R_LLAVE
            | L_LLAVE NOMBRE COMA ESTADO COMA FECHA_IN R_LLAVE COMA PROY
            | L_LLAVE NOMBRE COMA FECHA_IN R_LLAVE
            | L_LLAVE NOMBRE COMA FECHA_IN R_LLAVE COMA PROY'''


def p_NOMBRE_E(p):
    '''NOMBRE_E : t_nombre_empresa DOSPUNTOS STRING'''


def p_FUNDACION(p):
    '''FUNDACION : t_fundacion DOSPUNTOS INTEGER'''


def p_CALLE(p):
    '''CALLE : t_calle DOSPUNTOS STRING'''


def p_CIUDAD(p):
    '''CIUDAD : t_ciudad DOSPUNTOS STRING'''


def p_PAIS(p):
    '''PAIS : t_pais DOSPUNTOS STRING'''


def p_INGRESOS_AN(p):
    '''INGRESOS_AN : t_ingresos_anuales DOSPUNTOS FLOAT'''


def p_PYME(p):
    '''PYME : t_pyme DOSPUNTOS BOOLEAN'''

def p_LINK(p):
    '''LINK : t_link DOSPUNTOS LinkURL
            | t_link DOSPUNTOS VACIOS'''


def p_NOMBRE(p):
    '''NOMBRE : t_nombre DOSPUNTOS STRING'''

def p_JEFE(p):
    '''JEFE : t_jefe DOSPUNTOS STRING
            | t_jefe DOSPUNTOS VACIOS'''


def p_EDAD(p):
    '''EDAD : t_edad DOSPUNTOS INTEGER
            | t_edad DOSPUNTOS VACIOS'''

def p_CARGO(p):
    '''CARGO : t_cargo DOSPUNTOS POSIBLE_CARGO'''

def p_SALARIO(p):
    '''SALARIO : t_salario DOSPUNTOS INTEGER
               | t_salario DOSPUNTOS FLOAT'''


def p_ACTIVO(p):
    '''ACTIVO : t_activo DOSPUNTOS BOOLEAN'''


def p_FECHA_CON(p):
    '''FECHA_CON : t_fecha_contratacion DOSPUNTOS DATE'''

def p_ESTADO(p):
    '''ESTADO : t_estado DOSPUNTOS POSIBLE_ESTADO
              | t_estado DOSPUNTOS VACIOS'''

def p_FECHA_IN(p):
    '''FECHA_IN : t_fecha_inicio DOSPUNTOS DATE'''

def p_FECHA_FIN(p):
    '''FECHA_FIN : t_fecha_fin DOSPUNTOS DATE
                 | t_fecha_fin DOSPUNTOS VACIOS'''

def p_VERSION(p):
    '''VERSION : t_version DOSPUNTOS STRING
               | t_version DOSPUNTOS VACIOS'''

def p_FIRMADIG(p):
    '''FIRMADIG : t_firma_digital DOSPUNTOS STRING
                | t_firma_digital DOSPUNTOS VACIOS'''

def p_VACIOS(p):
    '''VACIOS : t_VACIOS'''
## errores sintácticos

def p_error(p):
    if p:
        pos = p.lexpos
        line_count = 1
        column_count = 1
        for i, char in enumerate(lexer.lexdata[:pos]):
            if char == '\n':
                line_count += 1
                column_count = 1
            else:
                column_count += 1
        print(f"Error de sintaxis en el token {p.type}, valor {p.value} en la línea {line_count}, columna {column_count}\n")
        
    else:
        print("Error de sintaxis al final del archivo")

parser = yacc.yacc()

def escape(text):
    return html.escape(str(text))

def generate_html_from_json(parsed_data, output_filename):
    with open(output_filename, 'w', encoding='utf-8') as f:
        f.write("<html>\n")
        f.write("  <head>\n")
        f.write("    <title>JSON to HTML</title>\n")
        f.write("    <style>\n")
        f.write("      body { font-family: Arial, sans-serif; background-color: #e0f7fa; }\n")
        f.write("      .company { border: 1px solid #80cbc4; margin-bottom: 10px; padding: 10px; background-color: #e0f7fa; border-radius: 5px; box-shadow: 0 0 10px rgba(0,0,0,0.1); }\n")
        f.write("      .company h1 { font-size: 20px; color: #00695c; margin-bottom: 10px; border-bottom: 1px solid #80cbc4; padding-bottom: 5px; }\n")
        f.write("      .department { margin-left: 20px; }\n")
        f.write("      .department h2 { font-size: 18px; color: #00897b; margin-bottom: 10px; border-bottom: 1px solid #80cbc4; padding-bottom: 5px; }\n")
        f.write("      .subdepartment { margin-left: 40px; }\n")
        f.write("      .subdepartment h3 { font-size: 16px; color: #26a69a; margin-bottom: 10px; border-bottom: 1px solid #80cbc4; padding-bottom: 5px; }\n")
        f.write("      .employees ul { list-style-type: none; padding-left: 0; }\n")
        f.write("      .projects { border-collapse: collapse; width: 100%; }\n")
        f.write("      .projects th, .projects td { border: 1px solid #80cbc4; padding: 8px; background-color: #b2dfdb; }\n")
        f.write("    </style>\n")
        f.write("  </head>\n")
        f.write("  <body>\n")

        for company in parsed_data['empresas']:
            f.write("    <div class='company'>\n")
            f.write(f"      <h1>{escape(company['nombre_empresa'])}</h1>\n")
            f.write(f"      <p>Fundación: {escape(company.get('fundacion', ''))}</p>\n")
            if 'direccion' in company and all(k in company['direccion'] for k in ('calle', 'ciudad', 'pais')):
                f.write(f"      <p>Dirección: {escape(company['direccion']['calle'])}, "
                        f"{escape(company['direccion']['ciudad'])}, "
                        f"{escape(company['direccion']['pais'])}</p>\n")
            f.write(f"      <p>Ingresos Anuales: {company['ingresos_anuales']}</p>\n")
            f.write(f"      <p>PYME: {'Sí' if company['pyme'] else 'No'}</p>\n")
            f.write(f"      <p><a href='{escape(company.get('link', ''))}'>Enlace</a></p>\n")

            if 'departamentos' in company:
                f.write("      <div class='departments'>\n")
                for department in company['departamentos']:
                    f.write("        <div class='department'>\n")
                    f.write(f"          <h2>{escape(department['nombre'])}</h2>\n")

                    if 'subdepartamentos' in department:
                        f.write("          <div class='subdepartments'>\n")
                        for subdepartment in department['subdepartamentos']:
                            f.write("            <div class='subdepartment'>\n")
                            f.write(f"              <h3>{escape(subdepartment['nombre'])}</h3>\n")

                            if 'empleados' in subdepartment:
                                f.write("              <div class='employees'>\n")
                                f.write("                <ul>\n")
                                for employee in subdepartment['empleados']:
                                    f.write(f"                  <li>{escape(employee['nombre'])}, "
                                            f"{employee.get('edad', '')} años, "
                                            f"{escape(employee.get('cargo', ''))}, "
                                            f"${employee.get('salario', '')}, "
                                            f"{'Activo' if employee.get('activo', False) else 'Inactivo'}, "
                                            f"Contratado el {employee.get('fecha_contratacion', '')}</li>\n")
                                    if 'proyectos' in employee and isinstance(employee['proyectos'], list):
                                        f.write("                  <table class='projects'>\n")
                                        f.write("                    <tr><th>Nombre del Proyecto</th><th>Estado</th>"
                                                "<th>Fecha de Inicio</th><th>Fecha de Fin</th></tr>\n")
                                        for project in employee['proyectos']:
                                            f.write(f"                    <tr><td>{escape(project.get('nombre', ''))}</td>"
                                                    f"<td>{escape(project.get('estado', ''))}</td>"
                                                    f"<td>{project.get('fecha_inicio', '')}</td>"
                                                    f"<td>{project.get('fecha_fin', '')}</td></tr>\n")
                                        f.write("                  </table>\n")
                                f.write("                </ul>\n")
                                f.write("              </div>\n")  # Cierre de 'employees'

                            f.write("            </div>\n")  # Cierre de 'subdepartment'
                        f.write("          </div>\n")  # Cierre de 'subdepartments'
                    f.write("        </div>\n")  # Cierre de 'department'
                f.write("      </div>\n")  # Cierre de 'departments'
            f.write("    </div>\n")  # Cierre de 'company'

        f.write("  </body>\n")
        f.write("</html>\n")

def parse_json_input(data):
    # Configurar el lexer
    lexer.input(data)

    while True:
        tok = lexer.token()
        if not tok:
            break
        line_start = data.rfind('\n', 0, tok.lexpos) + 1
        line = data.count('\n', 0, tok.lexpos) + 1
        column = tok.lexpos - line_start + 1
        print(f"Linea {line} | Tipo de Token: {tok.type}, Valor: {tok.value}")

    # Realizar análisis sintáctico
    result = parser.parse(data, lexer=lexer)
    if result is not None:
        print("El análisis sintáctico no detectó errores.")
    else:
        print("El análisis sintáctico detectó errores.")


def main():
    print("Selecciona una opción:")
    print("1. Introducir texto JSON directamente")
    print("2. Especificar el nombre de un archivo JSON o TXT")

    choice = input("Opción: ")

    if choice == "1":
        json_data = input("Introduce el texto JSON: ")
        parse_json_input(json_data)
        # Realizar análisis sintáctico y generar HTML si no hay errores
        try:
            parsed_data = json.loads(json_data)
            output_filename = "output.html"
            generate_html_from_json(parsed_data, output_filename)
            print(f"Generación de HTML exitosa: {output_filename}")
        except json.JSONDecodeError as e:
            print(f"Error al decodificar JSON: {e}")
    elif choice == "2":
        file_path = input("Introduce el nombre del archivo JSON o TXT: ")
        if os.path.isfile(file_path):
            with open(file_path, 'r', encoding='utf-8') as file:
                json_data = file.read()
                parse_json_input(json_data)
                # Realizar análisis sintáctico y generar HTML si no hay errores
                try:
                    parsed_data = json.loads(json_data)
                    output_filename = "output.html"
                    generate_html_from_json(parsed_data, output_filename)
                    print(f"Generación de HTML exitosa: {output_filename}")
                except json.JSONDecodeError as e:
                    print(f"Error al decodificar JSON: {e}")
        else:
            print("El archivo especificado no existe.")
    else:
        print("Opción no válida.")

if __name__ == "__main__":
    main()