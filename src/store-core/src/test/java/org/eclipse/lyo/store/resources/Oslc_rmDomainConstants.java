package org.eclipse.lyo.store.resources;

public interface Oslc_rmDomainConstants
{
    public static String REQUIREMENTS_MANAGEMENT_DOMAIN = "http://open-services.net/ns/rm#";
    public static String REQUIREMENTS_MANAGEMENT_NAMSPACE = "http://open-services.net/ns/rm#";
    public static String REQUIREMENTS_MANAGEMENT_NAMSPACE_PREFIX = "oslc_rm";

    public static String REQUIREMENT_PATH = "requirement";
    public static String REQUIREMENT_NAMESPACE = REQUIREMENTS_MANAGEMENT_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String REQUIREMENT_LOCALNAME = "Requirement"; //localName of the rdfs:class the resource describes
    public static String REQUIREMENT_TYPE = REQUIREMENT_NAMESPACE + REQUIREMENT_LOCALNAME; //fullname of the rdfs:class the resource describes
    public static String REQUIREMENTCOLLECTION_PATH = "requirementCollection";
    public static String REQUIREMENTCOLLECTION_NAMESPACE = REQUIREMENTS_MANAGEMENT_NAMSPACE; //namespace of the rdfs:class the resource describes
    public static String REQUIREMENTCOLLECTION_LOCALNAME = "RequirementCollection"; //localName of the rdfs:class the resource describes
    public static String REQUIREMENTCOLLECTION_TYPE = REQUIREMENTCOLLECTION_NAMESPACE + REQUIREMENTCOLLECTION_LOCALNAME; //fullname of the rdfs:class the resource describes
}
