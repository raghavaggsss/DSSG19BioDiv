# This is an auto-generated Django model module.
# You'll have to do the following manually to clean this up:
#   * Rearrange models' order
#   * Make sure each model has one field with primary_key=True
#   * Make sure each ForeignKey has `on_delete` set to the desired behavior.
#   * Remove `managed = False` lines if you wish to allow Django to create, modify, and delete the table
# Feel free to rename the models, but don't rename db_table values or field names.
from django.contrib.gis.db import models

class GbifSummary(models.Model):
    datasetname = models.TextField(default=None)
    common = models.TextField(default=None)
    redlist = models.TextField(default=None)
    t_kingdom = models.TextField(default=None)
    t_phylum = models.TextField(default=None)
    t_class = models.TextField(default=None)
    t_order = models.TextField(default=None)
    t_family = models.TextField(default=None)
    t_genus = models.TextField(default=None)
    species = models.TextField(default=None)
    recency = models.TextField(default=None)
    lon = models.DecimalField(max_digits=30, decimal_places=6, default=None)
    lat = models.DecimalField(max_digits=30, decimal_places=6, default=None)
    point = models.PointField(spatial_index=True)

class GbifSummaryFull(models.Model):
    datasetname = models.TextField(default=None)
    species = models.TextField(default=None)
    lon = models.DecimalField(max_digits=30, decimal_places=6, default=None)
    lat = models.DecimalField(max_digits=30, decimal_places=6, default=None)
    point = models.PointField(spatial_index=True)
    year = models.IntegerField(max_length=4, default=None)