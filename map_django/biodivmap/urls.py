from django.urls import path

from . import views

urlpatterns = [
    path('', views.index, name="index"),
    path('species/', views.ajax_species, name="species"),
    path('taxontree/', views.taxontree, name="taxontree")
]
