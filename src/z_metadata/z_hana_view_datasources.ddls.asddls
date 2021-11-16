@AbapCatalog.sqlViewName: 'ZHANAVIEWDSS'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'Hana View Datasources'
define view Z_HANA_VIEW_DATASOURCES 
as select 
from zmetadatadom as lev1
left outer join zmetadatadom lev2 on lev1.node_id = lev2.parent_node_id
and lev1.objectname = lev2.objectname
left outer join zmetadatadom lev3 on lev2.node_id = lev3.parent_node_id 
and lev2.objectname = lev3.objectname
 {
lev1.objectname as CalculationView,
lev1.node_id ,
           max(case when lev2.node_name = 'id' then lev2.node_value else '' end) as id_datasource,
           max(case when lev2.node_name = 'type' then lev2.node_value else '' end) as type,
           max(case when lev3.node_name = '#text' then  substring(lev3.node_value, 30,50) else '' end) as CalculationView_Source,
           max(case when lev3.node_name = 'schemaName' then lev3.node_value else '' end) as schemaName,
           max(case when lev3.node_name = 'columnObjectName' then lev3.node_value else '' end) as columnObjectName

} where      lev1.node_type = 1            -- get us all elements
and        lev1.node_name  = 'DataSource'
and lev1.objecttype = 'CALCVIEW'
group by lev1.objectname, lev1.node_id
