<?xml version='1.0' encoding='ISO-8859-1' ?>
<!DOCTYPE helpset
  PUBLIC "-//Sun Microsystems Inc.//DTD JavaHelp HelpSet Version 1.0//EN"
         "http://java.sun.com/products/javahelp/helpset_1_0.dtd">

<helpset version="1.0">

<!-- title -->
 <title>GEOHELPv0.1 Help</title>

<!-- maps -->
<maps>
    <homeID>help</homeID>
    <mapref location="GEOHELPv0.1Map.jhm"/>
</maps>

<!-- views -->
<view>
    <name>TOC</name>
    <label>Table of Contents</label>
    <type>javax.help.TOCView</type>
    <data>GEOHELPv0.1TOC.xml</data>
</view>

<view>
    <name>Index</name>
    <label>Index</label>
    <type>javax.help.IndexView</type>
    <data>GEOHELPv0.1Index.xml</data>
</view>

<view>
    <name>Search</name>
    <label>Search</label>
    <type>javax.help.SearchView</type>
    <data engine="com.sun.java.help.search.DefaultSearchEngine">
      GEOHELPv0.1HelpSearch
    </data>
</view>

</helpset>