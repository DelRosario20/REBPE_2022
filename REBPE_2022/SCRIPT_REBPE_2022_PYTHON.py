#=========================================================================================================#
# Script para análisis de datos del Registro Estadístico Base de Población del Ecuador                    #
# Autor: Alan Del Rosario                                                                                 #
# Fecha: 18/05/2025                                                                                       #
# centrándose en el cantón Ambato (Código 1801).                                                          #
#=========================================================================================================#

import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from statsmodels.formula.api import ols
from statsmodels.stats.anova import anova_lm
from statsmodels.stats.multicomp import pairwise_tukeyhsd
from scipy.stats import chi2_contingency

# Configuración inicial
plt.style.use('seaborn')
sns.set_palette('pastel')

# Cargar datos
df = pd.read_csv("C:/Users/USER/Documents/REBPE_2022/Data/Bdd_Datos_Abiertos_REBPE_2022_/rebpe_2022.csv")
df = df[df['cant_res'] == 1801]

# Transformación de variables categóricas
categorical_maps = {
    'sexo': {1: 'Hombre', 2: 'Mujer', 3: 'Indeterminado'},
    'grupo_edad': {
        1: '0-4', 2: '5-9', 3: '10-14', 4: '15-19', 5: '20-24', 
        6: '25-29', 7: '30-34', 8: '35-39', 9: '40-44', 10: '45-49',
        11: '50-54', 12: '55-59', 13: '60-64', 14: '65-69', 
        15: '70-74', 16: '75-79', 17: '80-84', 18: '85+'
    },
    'e_civil': {
        1: 'Soltero/a', 2: 'Casado/a', 3: 'Divorciado/a', 
        4: 'Viudo/a', 5: 'Unión de hecho', 9: 'No registra'
    },
    'es_madre': {1: 'Sí', 2: 'No'},
    'es_padre': {1: 'Sí', 2: 'No'},
    'tiene_disc': {1: 'Sí', 2: 'No'},
    'tipo_disc': {
        1: 'Intelectual', 2: 'Física', 3: 'Visual', 
        4: 'Auditiva', 5: 'Psicosocial', 6: 'Lenguaje', 9: 'No registra'
    },
    'pensionistas': {1: 'Sí', 2: 'No'}
}

for col, mapping in categorical_maps.items():
    df[col] = df[col].map(mapping).astype('category')

# Tratamiento de valores faltantes
for col in ['es_madre', 'es_padre', 'tipo_disc', 'pensionistas']:
    df[col] = df[col].cat.add_categories('No Aplica').fillna('No Aplica')

df['anios_viud'] = df['anios_viud'].fillna(0)
df['edad_viud'] = df['edad_viud'].fillna(0)

#----------------------------------------------------------
# 1. Tablas cruzadas - Discapacidad por sexo
#----------------------------------------------------------
discapacitados = df[df['tiene_disc'] == 'Sí']
tabla_disc = pd.crosstab(discapacitados['sexo'], discapacitados['tipo_disc'])

plt.figure(figsize=(12,6))
sns.countplot(data=discapacitados, x='sexo', hue='tipo_disc')
plt.title('Distribución de tipos de discapacidad por sexo')
plt.xlabel('Sexo')
plt.ylabel('Conteo')
plt.legend(title='Tipo discapacidad')
plt.show()

#----------------------------------------------------------
# 2. Pensionistas por edad y sexo
#----------------------------------------------------------
pensionistas = df[df['pensionistas'] == 'Sí']
tabla_pension = pd.crosstab(pensionistas['grupo_edad'], pensionistas['sexo'])

plt.figure(figsize=(12,6))
sns.barplot(data=pensionistas, x='grupo_edad', y='edad', hue='sexo', estimator=len)
plt.title('Distribución de pensionistas por grupo de edad y sexo')
plt.xlabel('Grupo de edad')
plt.ylabel('Conteo')
plt.xticks(rotation=45)
plt.show()

#----------------------------------------------------------
# 3. Edad promedio de viudez
#----------------------------------------------------------
viudos = df[(df['e_civil'] == 'Viudo/a') & (df['edad_viud'] > 0)]
tabla_viudez = viudos.groupby(['grupo_edad', 'sexo'])['edad_viud'].mean().reset_index()

plt.figure(figsize=(12,6))
sns.lineplot(data=tabla_viudez, x='grupo_edad', y='edad_viud', hue='sexo', marker='o')
plt.title('Edad promedio de viudez por sexo y grupo de edad')
plt.xlabel('Grupo de edad')
plt.ylabel('Edad promedio')
plt.xticks(rotation=45)
plt.show()

#----------------------------------------------------------
# 4. Estado civil por edad y sexo
#----------------------------------------------------------
tabla_estado = pd.crosstab([df['grupo_edad'], df['sexo']], df['e_civil'], normalize='index')

g = sns.FacetGrid(tabla_estado.reset_index(), col='sexo', height=6, aspect=1.2)
g.map(sns.barplot, 'grupo_edad', 'proportion', 'e_civil')
plt.xticks(rotation=45)
plt.subplots_adjust(top=0.85)
g.fig.suptitle('Distribución del estado civil por grupo de edad y sexo')
plt.show()

#----------------------------------------------------------
# ANOVA y Tukey
#----------------------------------------------------------
# ANOVA 1: Edad por grupo etario
model = ols('edad ~ C(grupo_edad)', data=df).fit()
anova_results = anova_lm(model)
print(anova_results)

tukey = pairwise_tukeyhsd(df['edad'], df['grupo_edad'])
print(tukey.summary())

# ANOVA 2: Edad de viudez
model_viudez = ols('edad_viud ~ C(grupo_edad)', data=viudos).fit()
anova_viudez = anova_lm(model_viudez)
print(anova_viudez)

tukey_viudez = pairwise_tukeyhsd(viudos['edad_viud'], viudos['grupo_edad'])
print(tukey_viudez.summary())

#----------------------------------------------------------
# Chi-cuadrado
#----------------------------------------------------------
# Para maternidad
tabla_madre = pd.crosstab(df['grupo_edad'], df['es_madre'])
chi2, p, dof, expected = chi2_contingency(tabla_madre)
print(f"Chi2 Maternidad: {chi2}, p-valor: {p}")

# Para discapacidad
discapacidad = df[df['tiene_disc'] == 'Sí']
tabla_disc = pd.crosstab(discapacidad['grupo_edad'], discapacidad['tipo_disc'])
chi2, p, dof, expected = chi2_contingency(tabla_disc)
print(f"Chi2 Discapacidad: {chi2}, p-valor: {p}")

#----------------------------------------------------------
# Boxplots
#----------------------------------------------------------
plt.figure(figsize=(10,6))
sns.boxplot(data=pensionistas, x='sexo', y='edad')
plt.title('Edad de pensionistas por sexo')
plt.show()

plt.figure(figsize=(10,6))
sns.boxplot(data=viudos, x='sexo', y='edad_viud')
plt.title('Edad al enviudar por sexo')
plt.show()

#----------------------------------------------------------
# Correlación
#----------------------------------------------------------
numeric_vars = df[['edad', 'anios_viud', 'edad_viud']].corr()

plt.figure(figsize=(8,6))
sns.heatmap(numeric_vars, annot=True, cmap='coolwarm', fmt=".2f")
plt.title('Matriz de correlación')
plt.show()
