import pandas as pd
import geopandas as gpd
import sqlite3
import requests
import pickle
from shapely.geometry import Point


def dataSetNamesFromKey(df, read_pickle="", write_pickle=""):
    if (read_pickle!=""):
        conv = pd.read_pickle(read_pickle)
    else:
        k = df['datasetKey'].unique()
        n = ([])
        for key in k:
            url = "http://api.gbif.org/v1/dataset/" + key
            name = requests.get(url).json()['title']
            n.append(name)
        conv = dict(zip(k,n))

    if (write_pickle !=""):
        pickle_out = open(write_pickle,'wb')
        pickle.dump(conv, pickle_out)
        pickle_out.close()

    df['datasetName'] = df['datasetKey']
    df = df.replace({'datasetName': conv})
    df = df.drop(columns='datasetKey')
    return df

def addCommonNames(geo_df, itis_path):
    # df_commons['common'] = df_commons[['language', 'vernacular_name']].apply(lambda x: ':'.join(x), axis=1)
    # df_commons = df_commons.groupby(['tsn'])['common'].apply(', '.join).reset_index()
    db = sqlite3.connect(itis_path)
    df_commons = pd.read_sql_query("SELECT * from vernaculars", db)
    df_species = pd.read_sql_query("SELECT * from longnames", db)

    df_commons = df_commons[df_commons['language'] == "English"]
    df_commons = df_commons.rename({'vernacular_name': 'common'}, axis=1)
    df_commons = df_commons[['tsn', 'common']]

    df_species = df_species[df_species['completename'].isin(geo_df.species)]
    df_commons = df_commons[df_commons['tsn'].isin(df_species.tsn)]

    df_commons = df_commons.groupby(['tsn'])['common'].apply(', '.join).reset_index()
    df_species = df_species[df_species['tsn'].isin(df_commons.tsn)]

    df_common_species = df_species.merge(df_commons, on="tsn")
    df_common_species = df_common_species.rename({'completename': 'species'}, axis=1)
    df_common_species = df_common_species.drop(['tsn'], 1)

    species = set(geo_df.species.unique())
    species_in_itis = set(df_common_species.species.unique())
    diff = list(species - species_in_itis)
    diff = {df_common_species.columns[0]: diff, df_common_species.columns[1]: 'Common Name unknown'}
    df = pd.DataFrame.from_dict(diff)
    df_species = pd.concat([df_common_species, df], sort=False)

    geo_df = geo_df.merge(df_species, on="species")
    return geo_df

def redListHelper(x, df_red, red_sciNames):
    if x['species'] in red_sciNames.values:
        return df_red.loc[red_sciNames[red_sciNames == x['species']].index[0]].redlistCategory
    else:
        return float('nan')

def addRedList(df, red_path):
    df_red = pd.read_csv(red_path)
    red_sciNames = df_red.scientificName
    df['redList'] = df.apply(lambda x: redListHelper(x, df_red, red_sciNames), 1).values
    return df

def toGEOdf(df):
    geometry = [Point(xy) for xy in zip(df['decimalLongitude'], df['decimalLatitude'])]
    # fix coordinate system
    geo_df = gpd.GeoDataFrame(df, geometry=geometry, crs={'init': 'epsg:4326'})
    return geo_df
