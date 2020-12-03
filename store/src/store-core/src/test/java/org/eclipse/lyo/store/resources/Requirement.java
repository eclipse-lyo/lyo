package org.eclipse.lyo.store.resources;

import java.net.URI;
import java.net.URISyntaxException;
import java.util.Date;
import java.util.HashSet;

import org.eclipse.lyo.oslc4j.core.annotation.OslcDescription;
import org.eclipse.lyo.oslc4j.core.annotation.OslcName;
import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcReadOnly;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.Link;
import org.eclipse.lyo.oslc4j.core.model.Occurs;
import org.eclipse.lyo.oslc4j.core.model.ValueType;
import org.eclipse.lyo.store.resources.Oslc_rmDomainConstants;

@OslcNamespace(Oslc_rmDomainConstants.REQUIREMENT_NAMESPACE)
@OslcName(Oslc_rmDomainConstants.REQUIREMENT_LOCALNAME)
@OslcResourceShape(title = "Requirement Resource Shape", describes = Oslc_rmDomainConstants.REQUIREMENT_TYPE)
public class Requirement
    extends AbstractResource
{
    private static final String DUBLIN_CORE_NAMSPACE = "http://purl.org/dc/terms/";

    private String title;
    private String description;
    private String identifier;
    private Date created;
    private HashSet<Link> decomposes = new HashSet<Link>();
    private String stringProperty;
    private Integer intProperty;

    public Requirement()
           throws URISyntaxException
    {
        super();
    }
    
    public Requirement(final URI about)
           throws URISyntaxException
    {
        super(about);
    }
    
    @OslcName("title")
    @OslcPropertyDefinition(DUBLIN_CORE_NAMSPACE + "title")
    @OslcDescription("Title of the resource represented as rich text in XHTML content. SHOULD include only content that is valid inside an XHTML <span> element.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.XMLLiteral)
    @OslcReadOnly(false)
    public String getTitle()
    {
        return title;
    }
    
    @OslcName("description")
    @OslcPropertyDefinition(DUBLIN_CORE_NAMSPACE + "description")
    @OslcDescription("Descriptive text about resource represented as rich text in XHTML content. SHOULD include only content that is valid and suitable inside an XHTML <div> element.")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.XMLLiteral)
    @OslcReadOnly(false)
    public String getDescription()
    {
        return description;
    }

    @OslcName("identifier")
    @OslcPropertyDefinition(DUBLIN_CORE_NAMSPACE + "identifier")
    @OslcDescription("A unique identifier for a resource. Typically read-only and assigned by the service provider when a resource is created. Not typically intended for end-user display.")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    public String getIdentifier()
    {
        return identifier;
    }

    @OslcName("created")
    @OslcPropertyDefinition(DUBLIN_CORE_NAMSPACE + "created")
    @OslcDescription("Timestamp of resource creation")
    @OslcOccurs(Occurs.ZeroOrOne)
    @OslcValueType(ValueType.DateTime)
    @OslcReadOnly(false)
    public Date getCreated()
    {
        return created;
    }
    
    @OslcName("decomposes")
    @OslcPropertyDefinition(Oslc_rmDomainConstants.REQUIREMENTS_MANAGEMENT_NAMSPACE + "decomposes")
    @OslcDescription("The object is decomposed by the subject.")
    @OslcOccurs(Occurs.ZeroOrMany)
    @OslcValueType(ValueType.Resource)
    @OslcReadOnly(false)
    public HashSet<Link> getDecomposes()
    {
        return decomposes;
    }
    
    @OslcName("stringProperty")
    @OslcPropertyDefinition(Nsp1DomainConstants.TESTDOMAIN_NAMSPACE + "stringProperty")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.String)
    @OslcReadOnly(false)
    public String getStringProperty()
    {
        return stringProperty;
    }
    
    @OslcName("intProperty")
    @OslcPropertyDefinition(Nsp1DomainConstants.TESTDOMAIN_NAMSPACE + "intProperty")
    @OslcOccurs(Occurs.ExactlyOne)
    @OslcValueType(ValueType.Integer)
    @OslcReadOnly(false)
    public Integer getIntProperty()
    {
        return intProperty;
    }

    public void setTitle(final String title )
    {
        this.title = title;
   
    }
    
    public void setDescription(final String description )
    {
        this.description = description;
    }
    
    public void setIdentifier(final String identifier )
    {
        this.identifier = identifier;
    }

    
    public void setCreated(final Date created )
    {
        this.created = created;
    }
    
    public void setDecomposes(final HashSet<Link> decomposes )
    {
        this.decomposes.clear();
        if (decomposes != null)
        {
            this.decomposes.addAll(decomposes);
        }
    }
    
    public void setStringProperty(final String stringProperty )
    {
        this.stringProperty = stringProperty;
    }
    
    public void setIntProperty(final Integer intProperty )
    {
        this.intProperty = intProperty;
    }
    
}
