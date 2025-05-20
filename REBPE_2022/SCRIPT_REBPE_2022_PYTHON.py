# Librerías
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt

# Cargar datos desde archivo CSV filtrando solo el cantón Ambato (1801)
datos = pd.read_csv("C:/Users/USER/Documents/REBPE_2022/Data/Bdd_Datos_Abiertos_REBPE_2022_/rebpe_2022.csv")
datos = datos[datos['cant_res'] == 1801]

# Revisar estructura general
print(datos.info())
print(datos.head())

# Definir mapeos de categorías
mapeos = {
    'sexo': {1: 'Hombre', 2: 'Mujer', 3: 'Indeterminado'},
    'grupo_edad': dict(zip(range(1, 19), [
        '0-4', '5-9', '10-14', '15-19', '20-24', '25-29', '30-34', '35-39',
        '40-44', '45-49', '50-54', '55-59', '60-64', '65-69', '70-74',
        '75-79', '80-84', '85+'
    ])),
    'etaedad': {1: 'Niñez (0-11)', 2: 'Adolescencia (12-17)', 3: 'Juventud (18-29)',
                4: 'Adultez (30-64)', 5: 'Adulto Mayor (65+)'},
    'e_civil': {1: 'Soltero/a', 2: 'Casado/a', 3: 'Divorciado/a', 4: 'Viudo/a',
                5: 'Unión de hecho', 9: 'No registra'},
    'es_madre': {1: 'Sí', 2: 'No'},
    'es_padre': {1: 'Sí', 2: 'No'},
    'padres_fall': {1: 'Ninguno', 2: 'Huérfano de padre', 3: 'Huérfano de madre', 4: 'Huérfano de ambos'},
    'tiene_disc': {1: 'Sí', 2: 'No'},
    'tipo_disc': {1: 'Intelectual', 2: 'Física', 3: 'Visual', 4: 'Auditiva',
                  5: 'Psicosocial', 6: 'Lenguaje', 9: 'No registra'},
    'pensionistas': {1: 'Sí', 2: 'No'}
}

# Aplicar mapeos a las variables categóricas
for var, mapping in mapeos.items():
    if var in datos.columns:
        datos[var] = datos[var].map(mapping).astype('category')

# Confirmar transformaciones
print(datos.info())
print(datos.describe(include='all'))

# Revisar estructura de transformaciones
print(datos.head())

# Revisar valores faltantes
missing_values = datos.isna().sum()
missing_values = missing_values[missing_values > 0].reset_index()
missing_values.columns = ['variable', 'missing_count']
print(missing_values)

# Comprobación rápida
print(datos.isna().sum())

# Tablas cruzadas

# ANOVA

# TUKEY

