import pandas as pd
import matplotlib.pyplot as plt
from sklearn.preprocessing import OneHotEncoder, MinMaxScaler
from imblearn.over_sampling import SMOTE
from sklearn.model_selection import train_test_split
from sklearn.model_selection import cross_val_score
from sklearn.preprocessing import LabelEncoder
from sklearn.ensemble import RandomForestClassifier
from sklearn.impute import SimpleImputer

path = "/home/artsiom/Работа/Учеба/ZIR/"
co2_production = pd.read_csv(path + 'co2_production.csv')
gni_per_capita = pd.read_csv(path + 'gross_national_income_per_capital.csv')
hdi = pd.read_csv(path + 'human_development_index.csv')
life_expectancy = pd.read_csv(path + 'life_expectancy_by_birth.csv')

all_data = life_expectancy.copy()

for dataframe in [co2_production, gni_per_capita, hdi]:
    all_data = all_data.merge(dataframe, on='Country', how='outer', suffixes=('', '_drop'))

all_data = all_data[all_data.columns.drop(list(all_data.filter(regex='_drop')))]

columns_to_drop = ['ISO3', 'hdicode', 'hdi_rank_2021']
all_data.drop(columns=columns_to_drop, inplace=True, errors='ignore')


all_data['avg_co2'] = all_data.filter(like='co2_prod_').mean(axis=1)
all_data['avg_gni'] = all_data.filter(like='gnipc_').mean(axis=1)  

for year in range(1990, 2022):
    co2_column_name = f"co2_prod_{year}"
    gni_column_name = f"gnipc_{year}"
    estimated_population_column_name = f"estimated_population_{year}"
    co2_per_capita_column_name = f"co2_per_capita_{year}"
    
    if co2_column_name in all_data.columns and gni_column_name in all_data.columns:
        all_data[estimated_population_column_name] = all_data[co2_column_name] / all_data[gni_column_name]
        all_data[co2_per_capita_column_name] = all_data[co2_column_name] / all_data[estimated_population_column_name]


print(all_data['avg_co2'].describe())
print(all_data['avg_gni'].describe())

all_data['co2_gni_interaction'] = all_data['avg_co2'] * all_data['avg_gni']


selected_countries = ["Poland", "Germany", "France"]

plt.figure(figsize=(15,7))
for country in selected_countries:
    country_data = all_data[all_data['Country'] == country]
    years = range(1990, 2022)
    co2_per_capita_values = []

    for year in years:
        col_name = f'co2_per_capita_{year}'
        if col_name in country_data.columns:
            co2_per_capita_values.append(country_data[col_name].values[0])
        else:
            co2_per_capita_values.append(None)

    available_years = [year for year, value in zip(years, co2_per_capita_values) if value is not None]
    available_values = [value for value in co2_per_capita_values if value is not None]

    plt.plot(available_years, available_values, label=country, marker='o')

plt.title("Wskaźnik emisji CO2 на mieszkańca w czasie")
plt.xlabel("Rok")
plt.ylabel("Emisja CO2 на mieszkańca")
plt.xticks(range(1990, 2022), rotation=45)
plt.grid(True, which="both", ls="--", linewidth=0.5)
plt.legend()
plt.tight_layout()
print(all_data['co2_gni_interaction'].describe())
print(all_data['hdi_2021'].describe())

plt.show()

plt.figure(figsize=(15, 7))

plt.scatter(all_data['co2_gni_interaction'], all_data['hdi_2021'], alpha=0.5)
plt.title('Interakcja między produkcją CO2 a GNI w stosunku do HDI w 2021')
plt.xlabel('Interakcja CO2-GNI')
plt.ylabel('HDI w 2021 roku')
plt.show()


for col in all_data.columns:
    if all_data[col].dtype == 'object':
        all_data[col].fillna(all_data[col].mode()[0], inplace=True)
    else:
        all_data[col].fillna(all_data[col].mean(), inplace=True)

encoder = OneHotEncoder(drop='first')
encoded_region = encoder.fit_transform(all_data[['region']])
encoded_region_df = pd.DataFrame(encoded_region.toarray(), columns=encoder.get_feature_names_out(['region']))
all_data = pd.concat([all_data.drop(['region'], axis=1), encoded_region_df], axis=1)


numeric_columns = all_data.select_dtypes(include=['float64', 'int64']).columns
scaler = MinMaxScaler()
all_data[numeric_columns] = scaler.fit_transform(all_data[numeric_columns])

all_data.drop_duplicates(inplace=True)

threshold = 0.7 * len(all_data.columns)  
all_data = all_data.dropna(thresh=threshold)

le = LabelEncoder()
all_data['Country'] = le.fit_transform(all_data['Country'])

Q1 = all_data[numeric_columns].quantile(0.25)
Q3 = all_data[numeric_columns].quantile(0.75)
IQR = Q3 - Q1

all_data = all_data[~((all_data[numeric_columns] < (Q1 - 1.5 * IQR)) | (all_data[numeric_columns] > (Q3 + 1.5 * IQR))).any(axis=1)]

plt.figure(figsize=(15,7))
all_data[['avg_co2', 'avg_gni', 'co2_gni_interaction']].boxplot()
plt.title('Wykres pudełkowy dla wybranych atrybutów')
plt.show()


X = all_data.drop('hdi_2021', axis=1)
y = (all_data['hdi_2021'] > 0.7).astype(int)

X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

plt.figure(figsize=(12,6))
all_data['przed_skalowaniem'] = all_data['avg_co2']
all_data['po_skalowaniu'] = scaler.fit_transform(all_data[['avg_co2']])
all_data[['przed_skalowaniem', 'po_skalowaniu']].boxplot()
plt.title('Porównanie wartości przed i po skalowaniu')
plt.show()

encoded_region_df.sum().plot(kind='bar', figsize=(12,6))
plt.title('Liczba wartości dla kategorii po One-Hot Encoding')
plt.show()


cols_to_drop = ['avg_gni', 'co2_gni_interaction']
X_train = X_train.drop(columns=cols_to_drop)
X_test = X_test.drop(columns=cols_to_drop)

imputer = SimpleImputer(strategy='median')
X_train = imputer.fit_transform(X_train)
X_test = imputer.transform(X_test)

X_train = pd.DataFrame(X_train, columns=X.columns.drop(cols_to_drop))
X_test = pd.DataFrame(X_test, columns=X.columns.drop(cols_to_drop))

smote = SMOTE()
X_train_resampled, y_train_resampled = smote.fit_resample(X_train, y_train)

plt.figure(figsize=(7,5))
y_train_resampled.value_counts().plot(kind='bar', color=['skyblue', 'salmon'])
plt.title('Rozkład klasy po zastosowaniu SMOTE')
plt.xlabel('Klasa')
plt.ylabel('Liczba próbek')
plt.xticks([0,1], ['HDI > 0.7', 'HDI <= 0.7'], rotation=0)
plt.show()

clf = RandomForestClassifier()
scores = cross_val_score(clf, X_train_resampled, y_train_resampled, cv=5)
print("Accuracy: %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 2))

plt.figure(figsize=(10,6))
plt.bar(range(1, len(scores)+1), scores, color="skyblue")
plt.xlabel('Iteracja walidacji krzyżowej')
plt.ylabel('Dokładność (Accuracy)')
plt.title('Wyniki walidacji krzyżowej dla modelu klasyfikacyjnego')
plt.ylim(0, 1)  
plt.xticks(range(1, len(scores)+1))
plt.show()

print("Średnia dokładność: %0.2f (+/- %0.2f)" % (scores.mean(), scores.std() * 2))
