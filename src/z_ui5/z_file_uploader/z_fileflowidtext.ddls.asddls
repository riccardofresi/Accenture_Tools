@AbapCatalog.sqlViewName: 'ZILEFLOWIDTEXT'
@AbapCatalog.compiler.compareFilter: true
@AbapCatalog.preserveKey: true
@AccessControl.authorizationCheck: #CHECK
@EndUserText.label: 'File Flow ID Text'
@ObjectModel:{
              representativeKey: 'FileFlowId',
              dataCategory: #TEXT,
              usageType.sizeCategory: #XS,
              usageType.serviceQuality: #D,
              usageType.dataClass:#MIXED
              }
@OData.publish: true
@Search.searchable: true
define view Z_FILEFLOWIDTEXT as select from dd07v

{
    @ObjectModel.text.element: ['FlowDesc']
     @Search: {
           defaultSearchElement: true,
           fuzzinessThreshold: 0.8,
           ranking: #HIGH
          }
    key domvalue_l as FileFlowId,
    @Semantics.text: true
     @Search: {
           defaultSearchElement: true,
           fuzzinessThreshold: 0.8,
           ranking: #HIGH
          }
ddtext as FileText
} 
where domname = 'ZFILEFLOWD' and ddlanguage = 'E'
