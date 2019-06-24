from django.db import models

# Create your models here.

class Year(models.Model):
    species = models.CharField(max_length=100, primary_key=True)
    y_1990 = models.IntegerField(db_column="1990")
    y_1991 = models.IntegerField(db_column="1991")
    y_1992 = models.IntegerField(db_column="1992")

    def __str__(self):
        return self.species

