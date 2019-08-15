from django.shortcuts import render
from django.http import HttpResponse, JsonResponse
from django.shortcuts import render, render_to_response
from django.template import RequestContext
from django.views.decorators.csrf import csrf_exempt
# from .views_helpers import geojson_creater
from .models import GbifSummary

import json
import pandas as pd
from shapely.geometry import Point, Polygon
import geopandas as gpd
import math
import shapely.wkt

from django.contrib.gis.geos import Polygon as Polyrag

taxLevel = ['kingdom', 'phylum', 'class', 'order', 'family', 'genus', 'species', 'end']
# df_map = pd.read_pickle("biodivmap/gbif_map.pkl")
# df_map = df_map.drop(['Unnamed: 0', 'Winter', 'Spring', 'Summer', 'Fall'], 1)
#
# geometry = [Point(xy) for xy in zip(df_map['decimalLongitude'], df_map['decimalLatitude'])]
# geo_df_map = gpd.GeoDataFrame(df_map, geometry=geometry, crs={'init': 'epsg:4326'})
# spatial_index_map = geo_df_map.sindex

# df_obs = pd.read_pickle("biodivmap/gbif_summary.pkl")

# TODO: store sindex outside and use full observations
# geometry = [Point(xy) for xy in zip(df_obs['decimalLongitude'], df_obs['decimalLatitude'])]
# geo_df_obs = gpd.GeoDataFrame(df_obs, geometry=geometry, crs={'init': 'epsg:4326'})
# spatial_index_obs = geo_df_obs.sindex
# geo_df_obs = geo_df_map
# spatial_index_obs = spatial_index_map

df_taxon = pd.read_csv("biodivmap/Taxonomy Freq.csv", encoding="latin1")

# predicting species in sei
species = pd.read_csv("biodivmap/species_parameters.csv")
species.index = species["Unnamed: 0"]
species = species.drop(["Unnamed: 0"], 1)
species = species.rename(columns={"(Intercept)": "Intercept"})
sei = pd.read_csv("biodivmap/sei_poly_info.csv")
sei.index = sei.SEI_PolyNb
sei = sei.drop(["SEI_PolyNb"], 1)



def generate_taxon_hierarchy(df, taxLevelIndex, prevIndex):
    list_dicts = []
    if taxLevel[taxLevelIndex] == 'end':
        return ([], 0)
    if taxLevel[taxLevelIndex] == 'kingdom':
        df_size = df.shape[0]
        gp = df.groupby(taxLevel[taxLevelIndex])

    else:
        df_size = df[df[taxLevel[taxLevelIndex - 1]] == prevIndex].shape[0]
        gp = df[df[taxLevel[taxLevelIndex - 1]] == prevIndex].groupby(taxLevel[taxLevelIndex])

    indexes = gp.size().index.to_list()
    values = list(gp.size().values)
    num_types = len(gp)
    ind_val = zip(indexes, values)
    ind_val = sorted(ind_val, key=lambda tup: tup[1], reverse=True)
    for i in ind_val:
        index = i[0]
        value = i[1]
        next_data, num_types_next = generate_taxon_hierarchy(df, taxLevelIndex + 1, index)
        curr_gp = gp.get_group(index)
        redList = 0
        if (curr_gp.redList.unique().shape[0] > 1 or not pd.isna(curr_gp.redList.unique()[0])):
            redList = 1
        if (taxLevel[taxLevelIndex] == "species"):

            list_dicts.append({"name": index,
                               "value": int(curr_gp.iloc[0].mun_freq),
                               "size": int(value), "children": next_data,
                               "taxLevel": taxLevel[taxLevelIndex],
                               "ratio": (1.0 * value) / df_size,
                               "types": int(num_types_next),
                               "size_tree": int(value),
                               "redList": redList,
                               })
        else:
            list_dicts.append({"name": index,

                               "children": next_data,
                               "taxLevel": taxLevel[taxLevelIndex],
                               "ratio": (1.0 * value) / df_size,
                               "types": int(num_types_next),
                               "size_tree": int(value),
                               "redList": redList,
                               })

    return (list_dicts, num_types)




def index(request):
    return render(request, 'biodivmap/index.html')

def home(request):
    return render(request, 'biodivmap/home.html')

@csrf_exempt
def ajax_species(request):
    if request.method == 'POST':
        if request.body:
            taxons_regions = json.loads(request.body)
            print(taxons_regions)
            if not bool(taxons_regions["taxons"]):
                return JsonResponse("no selection", safe=False)
            # bbox = taxons_regions["bbox"]
            # min_x = min([bbox[0], bbox[2]])
            # max_x = max([bbox[0], bbox[2]])
            # min_y = min([bbox[1], bbox[3]])
            # max_y = max([bbox[1], bbox[3]])
            coords = taxons_regions["polygon"]["geometry"]["coordinates"][0]
            # poly = Polygon(coords)

            qset = GbifSummary.objects.filter(point__within=Polyrag(coords))
            df = pd.DataFrame(list(qset.values()))
            df_region = df.rename(columns={'t_kingdom': 'kingdom', 't_phylum': 'phylum', 't_class': 'class',
                                               't_order': 'order', 't_family': 'family', 't_genus': 'genus',
                                           'lat': 'decimalLatitude', 'lon': 'decimalLongitude'})
            geometry = [Point(xy) for xy in zip(df_region['decimalLongitude'], df_region['decimalLatitude'])]
            df_region = gpd.GeoDataFrame(df_region, geometry=geometry, crs={'init': 'epsg:4326'})


            # spatial_index_map = request.session.get('spatial_index_map')
            # if not spatial_index_map:
            #     spatial_index_map = geo_df_map.sindex
            #
            # possible_matches_index = list(spatial_index_map.intersection(poly.bounds))
            # possible_matches = geo_df_map.iloc[possible_matches_index]
            # df_region = possible_matches[possible_matches.intersects(poly)]
            # df_region = df_region.drop(["geometry"], 1)

            # df_region = df_map[(df_map["decimalLatitude"] > min_y) & (df_map["decimalLongitude"] > min_x) &
            #                            (df_map["decimalLatitude"] < max_y) & (df_map["decimalLongitude"] < max_x)]

            df_out = gpd.GeoDataFrame(columns=df_region.columns)
            for taxon_name, info_dict in taxons_regions["taxons"].items():
                if df_region[df_region[info_dict["taxLevel"]] == taxon_name].shape[0] > 0:
                    df_out = df_out.append(df_region[df_region[info_dict["taxLevel"]] == taxon_name])

            # geometry = [Point(xy) for xy in zip(df_out['decimalLongitude'], df_out['decimalLatitude'])]
            # # fix coordinate system
            # geo_df = gpd.GeoDataFrame(df_out, geometry=geometry, crs={'init': 'epsg:4326'})
            df_out = df_out.drop(["decimalLongitude", "decimalLatitude", 'kingdom', 'phylum', 'class',
                        'order', 'family', 'genus', "point"], 1)
            if df_out.shape[0] > 0:
                # df_out.to_file("biodivmap/static/biodivmap/curr.geojson", driver="GeoJSON")
                json_out = df_out.to_json()
                return JsonResponse({"status":"success", "data": json_out}, safe=False)
            else:
                return JsonResponse({"status":"no occurrence", "data": null}, safe=False)

    return JsonResponse("success", safe=False)

# @csrf_exempt
# def show_summary(request):
#     if request.method == 'POST':
#         if request.body:
#             selected_regions = json.loads(request.body)
#             # assume municipality json created as bar_sunburst.json
#             #filter dataframe with municipality
#
#             if "municipality" in selected_regions.keys():
#                 df_obs_region = df_obs[df_obs['municipality'] == selected_regions["municipality"]]
#
#             else:
#                 bbox = selected_regions["bbox"]
#                 min_x = min([bbox[0], bbox[2]])
#                 max_x = max([bbox[0], bbox[2]])
#                 min_y = min([bbox[1], bbox[3]])
#                 max_y = max([bbox[1], bbox[3]])
#                 df_obs_region = df_obs[(df_obs["decimalLatitude"] > min_y) & (df_obs["decimalLongitude"] > min_x) &
#                                        (df_obs["decimalLatitude"] < max_y) & (df_obs["decimalLongitude"] < max_x)]
#
#                 # bbox_poly = Polygon([(min_x, min_y), (max_x, min_y), (max_x, max_y), (min_x, max_y)])
#                 # geometry = [Point(xy) for xy in zip(df_obs['decimalLongitude'], df_obs['decimalLatitude'])]
#                 # geo_df = gpd.GeoDataFrame(df_obs, geometry=geometry, crs={'init': 'epsg:4326'})
#                 # df_obs_region = geo_df[geo_df.geometry.within(bbox_poly)]
#
#             df_taxon_region = df_taxon[df_taxon["species"].isin(df_obs_region['species'])]
#             df_taxon_region = df_taxon_region.set_index("species")
#             # species_gp = df_obs_mun.groupby(["species"])
#             # for spec, gp_spec in species_gp:
#             #     df_taxon_mun.loc[df_taxon_mun.loc[df_taxon_mun['species'] == spec].index, "freq"] = len(gp_spec)
#             # print(df_taxon_mun.shape)
#             df_freq = df_obs_region["species"].value_counts().to_frame()
#             df_freq.columns = ["mun_freq"]
#             df = pd.merge(df_taxon_region, df_freq, left_index=True, right_index=True)
#
#             # create jsons for sunburst and bar chart
#             df["species"] = df.index
#             df[['kingdom', 'phylum', 'class',
#                 'order', 'family', 'genus', 'species']] = df[['kingdom',
#                                                               'phylum', 'class', 'order', 'family',
#                                                    'genus', 'species']].fillna(value="Unknown")
#             json_dict, num_types = generate_plot_json(df, 0, "blah")
#             json_dict = {"name": "Organisms", "children": json_dict, "taxLevel": "organisms"}
#
#             with open('biodivmap/static/biodivmap/bar_sunburst.json', 'w') as fp:
#                 json.dump(json_dict, fp)
#
#             json_dict_2, num_types_2 = generate_tree_json(df,0, "blah")
#             json_dict_2 = { "name": "Organisms", 'taxLevel': "organisms", "types": num_types_2,
#                          "children": json_dict_2, "ratio": 1.0,
#                          "size": df.shape[0], "redList": 1}
#
#             with open('biodivmap/static/biodivmap/treedata_curr.json', 'w') as fp:
#                 json.dump(json_dict_2, fp)
#
#     return JsonResponse(["yo"], safe=False)


@csrf_exempt
def summary_polygon(request):
    if request.method == 'POST':
        if request.body:
            req = json.loads(request.body)
            polygon_json = req["shape"]
            coords = polygon_json["geometry"]["coordinates"][0]
            # poly = Polygon(coords)

            qset = GbifSummary.objects.filter(point__within=Polyrag(coords))
            df = pd.DataFrame(list(qset.values()))
            df_obs_region = df.rename(columns={'t_kingdom': 'kingdom', 't_phylum': 'phylum', 't_class': 'class',
                                    't_order': 'order', 't_family': 'family', 't_genus': 'genus'})

            if df_obs_region.shape[0] == 0:
                df_obs_region = pd.DataFrame(columns=['common', 'datasetname', 'id', 'lat', 'lon', 'point', 'recency',
                       'redlist', 'species', 'class', 'family', 'genus', 'kingdom', 'order',
                       'phylum'])

            # spatial_index_obs = request.session.get('spatial_index_obs')
            # if not spatial_index_obs:
            #     spatial_index_obs = geo_df_obs.sindex
            # possible_matches_index = list(spatial_index_obs.intersection(poly.bounds))
            # possible_matches = geo_df_obs.iloc[possible_matches_index]
            # df_obs_region = possible_matches[possible_matches.intersects(poly)]


            df_taxon_region = df_taxon[df_taxon["species"].isin(df_obs_region['species'])]
            df_taxon_region = df_taxon_region.set_index("species")
            # species_gp = df_obs_mun.groupby(["species"])
            # for spec, gp_spec in species_gp:
            #     df_taxon_mun.loc[df_taxon_mun.loc[df_taxon_mun['species'] == spec].index, "freq"] = len(gp_spec)
            # print(df_taxon_mun.shape)
            df_freq = df_obs_region["species"].value_counts().to_frame()
            df_freq.columns = ["mun_freq"]
            df = pd.merge(df_taxon_region, df_freq, left_index=True, right_index=True)

            # create jsons for sunburst and bar chart
            df["species"] = df.index
            df[['kingdom', 'phylum', 'class',
                'order', 'family', 'genus', 'species']] = df[['kingdom',
                                                              'phylum', 'class', 'order', 'family',
                                                              'genus', 'species']].fillna(value="Unknown")
            json_dict, num_types = generate_taxon_hierarchy(df, 0, "blah")
            json_dict = {"name": "Organisms", "children": json_dict, "taxLevel": "organisms", "types": num_types,
                         "ratio": 1.0, "size_tree": df.shape[0], "redList": 1 }

            sei_index = req["sei_index"]
            if not sei_index:
                return JsonResponse({"summary": json_dict}, safe=False)
            else:
                specs = list(species.dot(sei.loc[int(sei_index)]).sort_values(ascending=False).index)
                probs = list(species.dot(sei.loc[int(sei_index)]).sort_values(ascending=False).values)
                df_species = df.species
                records_list = []
                for i in range(0, len(specs)):
                    observed = "no"
                    if specs[i] in df_species:
                        observed = "yes"
                    exp_odds = math.exp(probs[i])
                    prob = exp_odds / (1 + exp_odds)
                    records_list.append({"rank": i + 1, "species": specs[i], "observed": observed, "prob": prob})
                table_json = {"records": records_list, "queryRecordCount": len(specs),
                              "totalRecordCount": len(specs)}

                return JsonResponse({"summary": json_dict, "pred": table_json}, safe=False)


#
#
# @csrf_exempt
# def predict(request):
#     if request.method == 'POST':
#         if request.body:
#             poly_index = json.loads(request.body)
#             print(poly_index["sei_index"])
#             specs = list(species.dot(sei.loc[int(poly_index["sei_index"])]).sort_values(ascending=False).index)
#             probs = list(species.dot(sei.loc[int(poly_index["sei_index"])]).sort_values(ascending=False).values)
#
#             records_list = []
#             for i in range(0, len(specs)):
#                 records_list.append({"rank": i+1, "species": specs[i], "observed": "yes", "odds": probs[i]})
#             table_json = {"records": records_list, "queryRecordCount": len(specs),
#                             "totalRecordCount": len(specs)}
#
#             return JsonResponse(table_json, safe=False)
#
#     return JsonResponse(["yo"], safe=False)
