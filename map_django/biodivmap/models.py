# This is an auto-generated Django model module.
# You'll have to do the following manually to clean this up:
#   * Rearrange models' order
#   * Make sure each model has one field with primary_key=True
#   * Make sure each ForeignKey has `on_delete` set to the desired behavior.
#   * Remove `managed = False` lines if you wish to allow Django to create, modify, and delete the table
# Feel free to rename the models, but don't rename db_table values or field names.
from django.db import models


class SpeciesYear(models.Model):
    species = models.TextField(primary_key=True)
    year_1700 = models.TextField(blank=True, null=True)
    year_1800 = models.TextField(blank=True, null=True)
    year_1818 = models.TextField(blank=True, null=True)
    year_1856 = models.TextField(blank=True, null=True)
    year_1857 = models.TextField(blank=True, null=True)
    year_1859 = models.TextField(blank=True, null=True)
    year_1860 = models.TextField(blank=True, null=True)
    year_1872 = models.TextField(blank=True, null=True)
    year_1874 = models.TextField(blank=True, null=True)
    year_1875 = models.TextField(blank=True, null=True)
    year_1876 = models.TextField(blank=True, null=True)
    year_1877 = models.TextField(blank=True, null=True)
    year_1878 = models.TextField(blank=True, null=True)
    year_1879 = models.TextField(blank=True, null=True)
    year_1880 = models.TextField(blank=True, null=True)
    year_1881 = models.TextField(blank=True, null=True)
    year_1882 = models.TextField(blank=True, null=True)
    year_1883 = models.TextField(blank=True, null=True)
    year_1884 = models.TextField(blank=True, null=True)
    year_1885 = models.TextField(blank=True, null=True)
    year_1886 = models.TextField(blank=True, null=True)
    year_1887 = models.TextField(blank=True, null=True)
    year_1888 = models.TextField(blank=True, null=True)
    year_1889 = models.TextField(blank=True, null=True)
    year_1890 = models.TextField(blank=True, null=True)
    year_1891 = models.TextField(blank=True, null=True)
    year_1892 = models.TextField(blank=True, null=True)
    year_1893 = models.TextField(blank=True, null=True)
    year_1894 = models.TextField(blank=True, null=True)
    year_1895 = models.TextField(blank=True, null=True)
    year_1896 = models.TextField(blank=True, null=True)
    year_1897 = models.TextField(blank=True, null=True)
    year_1898 = models.TextField(blank=True, null=True)
    year_1899 = models.TextField(blank=True, null=True)
    year_1900 = models.TextField(blank=True, null=True)
    year_1901 = models.TextField(blank=True, null=True)
    year_1902 = models.TextField(blank=True, null=True)
    year_1903 = models.TextField(blank=True, null=True)
    year_1904 = models.TextField(blank=True, null=True)
    year_1905 = models.TextField(blank=True, null=True)
    year_1906 = models.TextField(blank=True, null=True)
    year_1907 = models.TextField(blank=True, null=True)
    year_1908 = models.TextField(blank=True, null=True)
    year_1909 = models.TextField(blank=True, null=True)
    year_1910 = models.TextField(blank=True, null=True)
    year_1911 = models.TextField(blank=True, null=True)
    year_1912 = models.TextField(blank=True, null=True)
    year_1913 = models.TextField(blank=True, null=True)
    year_1914 = models.TextField(blank=True, null=True)
    year_1915 = models.TextField(blank=True, null=True)
    year_1916 = models.TextField(blank=True, null=True)
    year_1917 = models.TextField(blank=True, null=True)
    year_1918 = models.TextField(blank=True, null=True)
    year_1919 = models.TextField(blank=True, null=True)
    year_1920 = models.TextField(blank=True, null=True)
    year_1921 = models.TextField(blank=True, null=True)
    year_1922 = models.TextField(blank=True, null=True)
    year_1923 = models.TextField(blank=True, null=True)
    year_1924 = models.TextField(blank=True, null=True)
    year_1925 = models.TextField(blank=True, null=True)
    year_1926 = models.TextField(blank=True, null=True)
    year_1927 = models.TextField(blank=True, null=True)
    year_1928 = models.TextField(blank=True, null=True)
    year_1929 = models.TextField(blank=True, null=True)
    year_1930 = models.TextField(blank=True, null=True)
    year_1931 = models.TextField(blank=True, null=True)
    year_1932 = models.TextField(blank=True, null=True)
    year_1933 = models.TextField(blank=True, null=True)
    year_1934 = models.TextField(blank=True, null=True)
    year_1935 = models.TextField(blank=True, null=True)
    year_1936 = models.TextField(blank=True, null=True)
    year_1937 = models.TextField(blank=True, null=True)
    year_1938 = models.TextField(blank=True, null=True)
    year_1939 = models.TextField(blank=True, null=True)
    year_1940 = models.TextField(blank=True, null=True)
    year_1941 = models.TextField(blank=True, null=True)
    year_1942 = models.TextField(blank=True, null=True)
    year_1943 = models.TextField(blank=True, null=True)
    year_1944 = models.TextField(blank=True, null=True)
    year_1945 = models.TextField(blank=True, null=True)
    year_1946 = models.TextField(blank=True, null=True)
    year_1947 = models.TextField(blank=True, null=True)
    year_1948 = models.TextField(blank=True, null=True)
    year_1949 = models.TextField(blank=True, null=True)
    year_1950 = models.TextField(blank=True, null=True)
    year_1951 = models.TextField(blank=True, null=True)
    year_1952 = models.TextField(blank=True, null=True)
    year_1953 = models.TextField(blank=True, null=True)
    year_1954 = models.TextField(blank=True, null=True)
    year_1955 = models.TextField(blank=True, null=True)
    year_1956 = models.TextField(blank=True, null=True)
    year_1957 = models.TextField(blank=True, null=True)
    year_1958 = models.TextField(blank=True, null=True)
    year_1959 = models.TextField(blank=True, null=True)
    year_1960 = models.TextField(blank=True, null=True)
    year_1961 = models.TextField(blank=True, null=True)
    year_1962 = models.TextField(blank=True, null=True)
    year_1963 = models.TextField(blank=True, null=True)
    year_1964 = models.TextField(blank=True, null=True)
    year_1965 = models.TextField(blank=True, null=True)
    year_1966 = models.TextField(blank=True, null=True)
    year_1967 = models.TextField(blank=True, null=True)
    year_1968 = models.TextField(blank=True, null=True)
    year_1969 = models.TextField(blank=True, null=True)
    year_1970 = models.TextField(blank=True, null=True)
    year_1971 = models.TextField(blank=True, null=True)
    year_1972 = models.TextField(blank=True, null=True)
    year_1973 = models.TextField(blank=True, null=True)
    year_1974 = models.TextField(blank=True, null=True)
    year_1975 = models.TextField(blank=True, null=True)
    year_1976 = models.TextField(blank=True, null=True)
    year_1977 = models.TextField(blank=True, null=True)
    year_1978 = models.TextField(blank=True, null=True)
    year_1979 = models.TextField(blank=True, null=True)
    year_1980 = models.TextField(blank=True, null=True)
    year_1981 = models.TextField(blank=True, null=True)
    year_1982 = models.TextField(blank=True, null=True)
    year_1983 = models.TextField(blank=True, null=True)
    year_1984 = models.TextField(blank=True, null=True)
    year_1985 = models.TextField(blank=True, null=True)
    year_1986 = models.TextField(blank=True, null=True)
    year_1987 = models.TextField(blank=True, null=True)
    year_1988 = models.TextField(blank=True, null=True)
    year_1989 = models.TextField(blank=True, null=True)
    year_1990 = models.TextField(blank=True, null=True)
    year_1991 = models.TextField(blank=True, null=True)
    year_1992 = models.TextField(blank=True, null=True)
    year_1993 = models.TextField(blank=True, null=True)
    year_1994 = models.TextField(blank=True, null=True)
    year_1995 = models.TextField(blank=True, null=True)
    year_1996 = models.TextField(blank=True, null=True)
    year_1997 = models.TextField(blank=True, null=True)
    year_1998 = models.TextField(blank=True, null=True)
    year_1999 = models.TextField(blank=True, null=True)
    year_2000 = models.TextField(blank=True, null=True)
    year_2001 = models.TextField(blank=True, null=True)
    year_2002 = models.TextField(blank=True, null=True)
    year_2003 = models.TextField(blank=True, null=True)
    year_2004 = models.TextField(blank=True, null=True)
    year_2005 = models.TextField(blank=True, null=True)
    year_2006 = models.TextField(blank=True, null=True)
    year_2007 = models.TextField(blank=True, null=True)
    year_2008 = models.TextField(blank=True, null=True)
    year_2009 = models.TextField(blank=True, null=True)
    year_2010 = models.TextField(blank=True, null=True)
    year_2011 = models.TextField(blank=True, null=True)
    year_2012 = models.TextField(blank=True, null=True)
    year_2013 = models.TextField(blank=True, null=True)
    year_2014 = models.TextField(blank=True, null=True)
    year_2015 = models.TextField(blank=True, null=True)
    year_2016 = models.TextField(blank=True, null=True)
    year_2017 = models.TextField(blank=True, null=True)
    year_2018 = models.TextField(blank=True, null=True)
    year_2019 = models.TextField(blank=True, null=True)

    class Meta:
        managed = True
        db_table = 'species_year'

    def __str__(self):
        return self.species

    def select2element(self):
        return "{id:" + "'" + self.species.replace(" ", "_")  + "-speciesdb', text:" + "'" + self.species +"'" + "}"


