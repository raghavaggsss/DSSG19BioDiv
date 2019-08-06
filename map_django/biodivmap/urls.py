from django.urls import path

from . import views

urlpatterns = [
    path('', views.index, name="index"),
    path('species/', views.ajax_species, name="species"),
    # path('summary/', views.show_summary, name="summary"),
    path('summarypolygon/', views.summary_polygon, name="summary_polygon"),
    path('home/', views.home, name="home"),
    # path('predict/', views.predict, name="predict"),
]

