/*******************************************************************************
 * Copyright (c) 2012, 2013 IBM Corporation.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *	
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 * Contributors:
 *
 *	   Russell Boykin		- initial API and implementation
 *	   Alberto Giammaria	- initial API and implementation
 *	   Chris Peters			- initial API and implementation
 *	   Gianluca Bernardini	- initial API and implementation
 *******************************************************************************/
package org.eclipse.lyo.oslc4j.test;

import java.math.BigInteger;
import java.net.URI;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Date;
import java.util.HashSet;
import java.util.List;
import java.util.Set;
import java.util.SortedSet;
import java.util.TreeSet;

import org.eclipse.lyo.oslc4j.core.annotation.OslcNamespace;
import org.eclipse.lyo.oslc4j.core.annotation.OslcOccurs;
import org.eclipse.lyo.oslc4j.core.annotation.OslcPropertyDefinition;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRange;
import org.eclipse.lyo.oslc4j.core.annotation.OslcRepresentation;
import org.eclipse.lyo.oslc4j.core.annotation.OslcResourceShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueShape;
import org.eclipse.lyo.oslc4j.core.annotation.OslcValueType;
import org.eclipse.lyo.oslc4j.core.model.AbstractResource;
import org.eclipse.lyo.oslc4j.core.model.Occurs;
import org.eclipse.lyo.oslc4j.core.model.OslcConstants;
import org.eclipse.lyo.oslc4j.core.model.Representation;
import org.eclipse.lyo.oslc4j.core.model.ValueType;

@OslcNamespace(Constants.TEST_NAMESPACE)
@OslcResourceShape(title = "Test Resource Shape", describes = Constants.TYPE_TEST)
public final class Test
	   extends AbstractResource
{
	private final Collection<BigInteger> bigIntegerCollection = new ArrayList<BigInteger>();
	private final List<Boolean>			 booleanCollection	  = new ArrayList<Boolean>();
	private final Set<Byte>				 byteCollection		  = new HashSet<Byte>();
	private final SortedSet<Date>		 dateCollection		  = new TreeSet<Date>();
	private final ArrayList<Double>		 doubleCollection	  = new ArrayList<Double>();
	private final HashSet<Float>		 floatCollection	  = new HashSet<Float>();
	private final TreeSet<Integer>		 integerCollection	  = new TreeSet<Integer>();
	private final Collection<Long>		 longCollection		  = new ArrayList<Long>();
	private final Collection<Nested>	 nestedCollection	  = new ArrayList<Nested>();
	private final Collection<Short>		 shortCollection	  = new ArrayList<Short>();
	private final Collection<String>	 stringCollection	  = new ArrayList<String>();
	private final Collection<URI>		 uriCollection		  = new ArrayList<URI>();

	private final List<BigInteger> bigIntegerProperties		  = new ArrayList<BigInteger>();
	private final List<Boolean>	   booleanProperties		  = new ArrayList<Boolean>();
	private final List<Byte>	   byteProperties			  = new ArrayList<Byte>();
	private final List<Date>	   dateProperties			  = new ArrayList<Date>();
	private final List<Double>	   doubleProperties			  = new ArrayList<Double>();
	private final List<Float>	   floatProperties			  = new ArrayList<Float>();
	private final List<Integer>	   integerProperties		  = new ArrayList<Integer>();
	private final List<Long>	   longProperties			  = new ArrayList<Long>();
	private final List<Nested>	   nestedProperties			  = new ArrayList<Nested>();
	private final List<Boolean>	   primitiveBooleanProperties = new ArrayList<Boolean>();
	private final List<Byte>	   primitiveByteProperties	  = new ArrayList<Byte>();
	private final List<Double>	   primitiveDoubleProperties  = new ArrayList<Double>();
	private final List<Float>	   primitiveFloatProperties	  = new ArrayList<Float>();
	private final List<Integer>	   primitiveIntegerProperties = new ArrayList<Integer>();
	private final List<Long>	   primitiveLongProperties	  = new ArrayList<Long>();
	private final List<Short>	   primitiveShortProperties	  = new ArrayList<Short>();
	private final List<Short>	   shortProperties			  = new ArrayList<Short>();
	private final List<String>	   stringProperties			  = new ArrayList<String>();
	private final List<URI>		   uriProperties			  = new ArrayList<URI>();

	private BigInteger bigIntegerProperty;
	private Boolean	   booleanProperty;
	private Byte	   byteProperty;
	private Date	   dateProperty;
	private Double	   doubleProperty;
	private Float	   floatProperty;
	private String	   identifier;
	private Integer	   integerProperty;
	private Long	   longProperty;
	private Nested	   nestedProperty;
	private boolean	   primitiveBooleanProperty;
	private byte	   primitiveByteProperty;
	private double	   primitiveDoubleProperty;
	private float	   primitiveFloatProperty;
	private int		   primitiveIntegerProperty;
	private long	   primitiveLongProperty;
	private short	   primitiveShortProperty;
	private Short	   shortProperty;
	private String	   stringProperty;
	private URI		   uriProperty;
	private String	   xmlLiteralProperty;
	
	// Special double and float values (supported by both the Jena and JSON4J providers).
	private double	   primitiveDoublePositiveInfinityProperty;
	private double	   primitiveDoubleNegativeInfinityProperty;
	private double	   primitiveDoubleNaNProperty;
	private float	   primitiveFloatPositiveInfinityProperty;
	private float	   primitiveFloatNegativeInfinityProperty;
	private float	   primitiveFloatNaNProperty;
	private Double	   doublePositiveInfinityProperty;
	private Double	   doubleNegativeInfinityProperty;
	private Double	   doubleNaNProperty;
	private Float	   floatPositiveInfinityProperty;
	private Float	   floatNegativeInfinityProperty;
	private Float	   floatNaNProperty;

	public Test()
	{
		super();
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "bigIntegerCollection")
	public Collection<BigInteger> getBigIntegerCollection()
	{
		return new ArrayList<BigInteger>(bigIntegerCollection);
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "bigIntegerProperties")
	public BigInteger[] getBigIntegerProperties()
	{
		return bigIntegerProperties.toArray(new BigInteger[bigIntegerProperties.size()]);
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "bigIntegerProperty")
	public BigInteger getBigIntegerProperty()
	{
		return bigIntegerProperty;
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "booleanCollection")
	public List<Boolean> getBooleanCollection()
	{
		return new ArrayList<Boolean>(booleanCollection);
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "booleanProperties")
	public Boolean[] getBooleanProperties()
	{
		return booleanProperties.toArray(new Boolean[booleanProperties.size()]);
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "byteCollection")
	public Set<Byte> getByteCollection()
	{
		return new TreeSet<Byte>(byteCollection);
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "byteProperties")
	public Byte[] getByteProperties()
	{
		return byteProperties.toArray(new Byte[byteProperties.size()]);
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "byteProperty")
	public Byte getByteProperty()
	{
		return byteProperty;
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "dateCollection")
	public SortedSet<Date> getDateCollection()
	{
		return new TreeSet<Date>(dateCollection);
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "dateProperties")
	public Date[] getDateProperties()
	{
		return dateProperties.toArray(new Date[dateProperties.size()]);
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "dateProperty")
	public Date getDateProperty()
	{
		return dateProperty;
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "doubleCollection")
	public ArrayList<Double> getDoubleCollection()
	{
		return new ArrayList<Double>(doubleCollection);
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "doubleProperties")
	public Double[] getDoubleProperties()
	{
		return doubleProperties.toArray(new Double[doubleProperties.size()]);
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "doubleProperty")
	public Double getDoubleProperty()
	{
		return doubleProperty;
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "floatCollection")
	public HashSet<Float> getFloatCollection()
	{
		return new HashSet<Float>(floatCollection);
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "floatProperties")
	public Float[] getFloatProperties()
	{
		return floatProperties.toArray(new Float[floatProperties.size()]);
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "floatProperty")
	public Float getFloatProperty()
	{
		return floatProperty;
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "identifier")
	public String getIdentifier()
	{
		return identifier;
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "integerCollection")
	public TreeSet<Integer> getIntegerCollection()
	{
		return new TreeSet<Integer>(integerCollection);
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "integerProperties")
	public Integer[] getIntegerProperties()
	{
		return integerProperties.toArray(new Integer[integerProperties.size()]);
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "integerProperty")
	public Integer getIntegerProperty()
	{
		return integerProperty;
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "longCollection")
	public Collection<Long> getLongCollection()
	{
		return new ArrayList<Long>(longCollection);
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "longProperties")
	public Long[] getLongProperties()
	{
		return longProperties.toArray(new Long[longProperties.size()]);
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "longProperty")
	public Long getLongProperty()
	{
		return longProperty;
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "nestedCollection")
	@OslcRange(Constants.TYPE_NESTED)
	@OslcRepresentation(Representation.Inline)
	@OslcValueShape(OslcConstants.PATH_RESOURCE_SHAPES + "/" + Constants.PATH_NESTED)
	@OslcValueType(ValueType.LocalResource)
	public Collection<Nested> getNestedCollection()
	{
		return new ArrayList<Nested>(nestedCollection);
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "nestedProperties")
	@OslcRange(Constants.TYPE_NESTED)
	@OslcRepresentation(Representation.Inline)
	@OslcValueShape(OslcConstants.PATH_RESOURCE_SHAPES + "/" + Constants.PATH_NESTED)
	@OslcValueType(ValueType.LocalResource)
	public Nested[] getNestedProperties()
	{
		return nestedProperties.toArray(new Nested[nestedProperties.size()]);
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "nestedProperty")
	@OslcRange(Constants.TYPE_NESTED)
	@OslcRepresentation(Representation.Inline)
	@OslcValueShape(OslcConstants.PATH_RESOURCE_SHAPES + "/" + Constants.PATH_NESTED)
	@OslcValueType(ValueType.LocalResource)
	public Nested getNestedProperty()
	{
		return nestedProperty;
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "primitiveBooleanProperties")
	public boolean[] getPrimitiveBooleanProperties()
	{
		final boolean[] result = new boolean[primitiveBooleanProperties.size()];
		int index = 0;

		for (final Boolean primitiveBooleanProperty : primitiveBooleanProperties)
		{
			result[index++] = primitiveBooleanProperty.booleanValue();
		}

		return result;
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "primitiveByteProperties")
	public byte[] getPrimitiveByteProperties()
	{
		final byte[] result = new byte[primitiveByteProperties.size()];
		int index = 0;

		for (final Byte primitiveByteProperty : primitiveByteProperties)
		{
			result[index++] = primitiveByteProperty.byteValue();
		}

		return result;
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "primitiveByteProperty")
	public byte getPrimitiveByteProperty()
	{
		return primitiveByteProperty;
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "primitiveDoubleProperties")
	public double[] getPrimitiveDoubleProperties()
	{
		final double[] result = new double[primitiveDoubleProperties.size()];
		int index = 0;

		for (final Double primitiveDoubleProperty : primitiveDoubleProperties)
		{
			result[index++] = primitiveDoubleProperty.doubleValue();
		}

		return result;
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "primitiveDoubleProperty")
	public double getPrimitiveDoubleProperty()
	{
		return primitiveDoubleProperty;
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "primitiveFloatProperties")
	public float[] getPrimitiveFloatProperties()
	{
		final float[] result = new float[primitiveFloatProperties.size()];
		int index = 0;

		for (final Float primitiveFloatProperty : primitiveFloatProperties)
		{
			result[index++] = primitiveFloatProperty.floatValue();
		}

		return result;
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "primitiveFloatProperty")
	public float getPrimitiveFloatProperty()
	{
		return primitiveFloatProperty;
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "primitiveIntegerProperties")
	public int[] getPrimitiveIntegerProperties()
	{
		final int[] result = new int[primitiveIntegerProperties.size()];
		int index = 0;
		for (final Integer primitiveIntegerProperty : primitiveIntegerProperties)
		{
			result[index++] = primitiveIntegerProperty.intValue();
		}

		return result;
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "primitiveIntegerProperty")
	public int getPrimitiveIntegerProperty()
	{
		return primitiveIntegerProperty;
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "primitiveLongProperties")
	public long[] getPrimitiveLongProperties()
	{
		final long[] result = new long[primitiveLongProperties.size()];
		int index = 0;
		for (final Long primitiveLongProperty : primitiveLongProperties)
		{
			result[index++] = primitiveLongProperty.longValue();
		}

		return result;
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "primitiveLongProperty")
	public long getPrimitiveLongProperty()
	{
		return primitiveLongProperty;
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "primitiveShortProperties")
	public short[] getPrimitiveShortProperties()
	{
		final short[] result = new short[primitiveShortProperties.size()];
		int index = 0;
		for (final Short primitiveShortProperty : primitiveShortProperties)
		{
			result[index++] = primitiveShortProperty.shortValue();
		}

		return result;
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "primitiveShortProperty")
	public short getPrimitiveShortProperty()
	{
		return primitiveShortProperty;
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "shortCollection")
	public Collection<Short> getShortCollection()
	{
		return new ArrayList<Short>(shortCollection);
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "shortProperties")
	public Short[] getShortProperties()
	{
		return shortProperties.toArray(new Short[shortProperties.size()]);
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "shortProperty")
	public Short getShortProperty()
	{
		return shortProperty;
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "stringCollection")
	public Collection<String> getStringCollection()
	{
		return new ArrayList<String>(stringCollection);
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "stringProperties")
	public String[] getStringProperties()
	{
		return stringProperties.toArray(new String[stringProperties.size()]);
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "stringProperty")
	public String getStringProperty()
	{
		return stringProperty;
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "uriCollection")
	public Collection<URI> getUriCollection()
	{
		return new ArrayList<URI>(uriCollection);
	}

	@OslcOccurs(Occurs.OneOrMany)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "uriProperties")
	public URI[] getUriProperties()
	{
		return uriProperties.toArray(new URI[uriProperties.size()]);
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "uriProperty")
	public URI getUriProperty()
	{
		return uriProperty;
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "xmlLiteralProperty")
	@OslcValueType(ValueType.XMLLiteral)
	public String getXmlLiteralProperty()
	{
		return xmlLiteralProperty;
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "booleanProperty")
	public Boolean isBooleanProperty()
	{
		return booleanProperty;
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "primitiveBooleanProperty")
	public boolean isPrimitiveBooleanProperty()
	{
		return primitiveBooleanProperty;
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "primitiveDoublePositiveInfinityProperty")
	public double getPrimitiveDoublePositiveInfinityProperty()
	{
		return primitiveDoublePositiveInfinityProperty;
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "primitiveDoubleNegativeInfinityProperty")
	public double getPrimitiveDoubleNegativeInfinityProperty()
	{
		return primitiveDoubleNegativeInfinityProperty;
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "primitiveDoubleNaNProperty")
	public double getPrimitiveDoubleNaNProperty()
	{
		return primitiveDoubleNaNProperty;
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "primitiveFloatPositiveInfinityProperty")
	public float getPrimitiveFloatPositiveInfinityProperty()
	{
		return primitiveFloatPositiveInfinityProperty;
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "primitiveFloatNegativeInfinityProperty")
	public float getPrimitiveFloatNegativeInfinityProperty()
	{
		return primitiveFloatNegativeInfinityProperty;
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "primitiveFloatNaNProperty")
	public float getPrimitiveFloatNaNProperty()
	{
		return primitiveFloatNaNProperty;
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "doublePositiveInfinityProperty")
	public Double getDoublePositiveInfinityProperty()
	{
		return doublePositiveInfinityProperty;
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "doubleNegativeInfinityProperty")
	public Double getDoubleNegativeInfinityProperty()
	{
		return doubleNegativeInfinityProperty;
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "doubleNaNProperty")
	public Double getDoubleNaNProperty()
	{
		return doubleNaNProperty;
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "floatPositiveInfinityProperty")
	public Float getFloatPositiveInfinityProperty()
	{
		return floatPositiveInfinityProperty;
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "floatNegativeInfinityProperty")
	public Float getFloatNegativeInfinityProperty()
	{
		return floatNegativeInfinityProperty;
	}

	@OslcOccurs(Occurs.ExactlyOne)
	@OslcPropertyDefinition(Constants.TEST_DOMAIN + "floatNaNProperty")
	public Float getFloatNaNProperty()
	{
		return floatNaNProperty;
	}

	public void setPrimitiveDoublePositiveInfinityProperty(double primitiveDoublePositiveInfinityProperty)
	{
		this.primitiveDoublePositiveInfinityProperty = primitiveDoublePositiveInfinityProperty;
	}

	public void setPrimitiveDoubleNegativeInfinityProperty(double primitiveDoubleNegativeInfinityProperty)
	{
		this.primitiveDoubleNegativeInfinityProperty = primitiveDoubleNegativeInfinityProperty;
	}

	public void setPrimitiveDoubleNaNProperty(double primitiveDoubleNaNProperty)
	{
		this.primitiveDoubleNaNProperty = primitiveDoubleNaNProperty;
	}

	public void setPrimitiveFloatPositiveInfinityProperty(float primitiveFloatPositiveInfinityProperty)
	{
		this.primitiveFloatPositiveInfinityProperty = primitiveFloatPositiveInfinityProperty;
	}

	public void setPrimitiveFloatNegativeInfinityProperty(float primitiveFloatNegativeInfinityProperty)
	{
		this.primitiveFloatNegativeInfinityProperty = primitiveFloatNegativeInfinityProperty;
	}

	public void setPrimitiveFloatNaNProperty(float primitiveFloatNaNProperty)
	{
		this.primitiveFloatNaNProperty = primitiveFloatNaNProperty;
	}

	public void setDoublePositiveInfinityProperty(Double doublePositiveInfinityProperty)
	{
		this.doublePositiveInfinityProperty = doublePositiveInfinityProperty;
	}

	public void setDoubleNegativeInfinityProperty(Double doubleNegativeInfinityProperty)
	{
		this.doubleNegativeInfinityProperty = doubleNegativeInfinityProperty;
	}

	public void setDoubleNaNProperty(Double doubleNaNProperty)
	{
		this.doubleNaNProperty = doubleNaNProperty;
	}

	public void setFloatPositiveInfinityProperty(Float floatPositiveInfinityProperty)
	{
		this.floatPositiveInfinityProperty = floatPositiveInfinityProperty;
	}

	public void setFloatNegativeInfinityProperty(Float floatNegativeInfinityProperty)
	{
		this.floatNegativeInfinityProperty = floatNegativeInfinityProperty;
	}

	public void setFloatNaNProperty(Float floatNaNProperty)
	{
		this.floatNaNProperty = floatNaNProperty;
	}

	public void setBigIntegerCollection(final Collection<BigInteger> bigIntegerCollection)
	{
		this.bigIntegerCollection.clear();

		if (bigIntegerCollection != null)
		{
			this.bigIntegerCollection.addAll(bigIntegerCollection);
		}
	}

	public void setBigIntegerProperties(final BigInteger[] bigIntegerProperties)
	{
		this.bigIntegerProperties.clear();

		if (bigIntegerProperties != null)
		{
			this.bigIntegerProperties.addAll(Arrays.asList(bigIntegerProperties));
		}
	}

	public void setBigIntegerProperty(final BigInteger bigIntegerProperty)
	{
		this.bigIntegerProperty = bigIntegerProperty;
	}

	public void setBooleanCollection(final List<Boolean> booleanCollection)
	{
		this.booleanCollection.clear();

		if (booleanCollection != null)
		{
			this.booleanCollection.addAll(booleanCollection);
		}
	}

	public void setBooleanProperties(final Boolean[] booleanProperties)
	{
		this.booleanProperties.clear();

		if (booleanProperties != null)
		{
			this.booleanProperties.addAll(Arrays.asList(booleanProperties));
		}
	}

	public void setBooleanProperty(final Boolean booleanProperty)
	{
		this.booleanProperty = booleanProperty;
	}

	public void setByteCollection(final Set<Byte> byteCollection)
	{
		this.byteCollection.clear();

		if (byteCollection != null)
		{
			this.byteCollection.addAll(byteCollection);
		}
	}

	public void setByteProperties(final Byte[] byteProperties)
	{
		this.byteProperties.clear();

		if (byteProperties != null)
		{
			this.byteProperties.addAll(Arrays.asList(byteProperties));
		}
	}

	public void setByteProperty(final Byte byteProperty)
	{
		this.byteProperty = byteProperty;
	}

	public void setDateCollection(final SortedSet<Date> dateCollection)
	{
		this.dateCollection.clear();

		if (dateCollection != null)
		{
			this.dateCollection.addAll(dateCollection);
		}
	}

	public void setDateProperties(final Date[] dateProperties)
	{
		this.dateProperties.clear();

		if (dateProperties != null)
		{
			this.dateProperties.addAll(Arrays.asList(dateProperties));
		}
	}

	public void setDateProperty(final Date dateProperty)
	{
		this.dateProperty = dateProperty;
	}

	public void setDoubleCollection(final ArrayList<Double> doubleCollection)
	{
		this.doubleCollection.clear();

		if (doubleCollection != null)
		{
			this.doubleCollection.addAll(doubleCollection);
		}
	}

	public void setDoubleProperties(final Double[] doubleProperties)
	{
		this.doubleProperties.clear();

		if (doubleProperties != null)
		{
			this.doubleProperties.addAll(Arrays.asList(doubleProperties));
		}
	}

	public void setDoubleProperty(final Double doubleProperty)
	{
		this.doubleProperty = doubleProperty;
	}

	public void setFloatCollection(final HashSet<Float> floatCollection)
	{
		this.floatCollection.clear();

		if (floatCollection != null)
		{
			this.floatCollection.addAll(floatCollection);
		}
	}

	public void setFloatProperties(final Float[] floatProperties)
	{
		this.floatProperties.clear();

		if (floatProperties != null)
		{
			this.floatProperties.addAll(Arrays.asList(floatProperties));
		}
	}

	public void setFloatProperty(final Float floatProperty)
	{
		this.floatProperty = floatProperty;
	}

	public void setIdentifier(final String identifier)
	{
		this.identifier = identifier;
	}

	public void setIntegerCollection(final TreeSet<Integer> integerCollection)
	{
		this.integerCollection.clear();

		if (integerCollection != null)
		{
			this.integerCollection.addAll(integerCollection);
		}
	}

	public void setIntegerProperties(final Integer[] integerProperties)
	{
		this.integerProperties.clear();

		if (integerProperties != null)
		{
			this.integerProperties.addAll(Arrays.asList(integerProperties));
		}
	}

	public void setIntegerProperty(final Integer integerProperty)
	{
		this.integerProperty = integerProperty;
	}

	public void setLongCollection(final Collection<Long> longCollection)
	{
		this.longCollection.clear();

		if (longCollection != null)
		{
			this.longCollection.addAll(longCollection);
		}
	}

	public void setLongProperties(final Long[] longProperties)
	{
		this.longProperties.clear();

		if (longProperties != null)
		{
			this.longProperties.addAll(Arrays.asList(longProperties));
		}
	}

	public void setLongProperty(final Long longProperty)
	{
		this.longProperty = longProperty;
	}

	public void setNestedCollection(final Collection<Nested> nestedCollection)
	{
		this.nestedCollection.clear();

		if (nestedCollection != null)
		{
			this.nestedCollection.addAll(nestedCollection);
		}
	}

	public void setNestedProperties(final Nested[] nestedProperties)
	{
		this.nestedProperties.clear();

		if (nestedProperties != null)
		{
			this.nestedProperties.addAll(Arrays.asList(nestedProperties));
		}
	}

	public void setNestedProperty(final Nested nestedProperty)
	{
		this.nestedProperty = nestedProperty;
	}

	public void setPrimitiveBooleanProperties(final boolean[] primitiveBooleanProperties)
	{
		this.primitiveBooleanProperties.clear();

		if (primitiveBooleanProperties != null)
		{
			for (final boolean primitiveBooleanProperty : primitiveBooleanProperties)
			{
				this.primitiveBooleanProperties.add(Boolean.valueOf(primitiveBooleanProperty));
			}
		}
	}

	public void setPrimitiveBooleanProperty(final boolean primitiveBooleanProperty)
	{
		this.primitiveBooleanProperty = primitiveBooleanProperty;
	}

	public void setPrimitiveByteProperties(final byte[] primitiveByteProperties)
	{
		this.primitiveByteProperties.clear();

		if (primitiveByteProperties != null)
		{
			for (final byte primitiveByteProperty : primitiveByteProperties)
			{
				this.primitiveByteProperties.add(Byte.valueOf(primitiveByteProperty));
			}
		}
	}

	public void setPrimitiveByteProperty(final byte primitiveByteProperty)
	{
		this.primitiveByteProperty = primitiveByteProperty;
	}

	public void setPrimitiveDoubleProperties(final double[] primitiveDoubleProperties)
	{
		this.primitiveDoubleProperties.clear();

		if (primitiveDoubleProperties != null)
		{
			for (final double primitiveDoubleProperty : primitiveDoubleProperties)
			{
				this.primitiveDoubleProperties.add(Double.valueOf(primitiveDoubleProperty));
			}
		}
	}

	public void setPrimitiveDoubleProperty(final double primitiveDoubleProperty)
	{
		this.primitiveDoubleProperty = primitiveDoubleProperty;
	}

	public void setPrimitiveFloatProperties(final float[] primitiveFloatProperties)
	{
		this.primitiveFloatProperties.clear();

		if (primitiveFloatProperties != null)
		{
			for (final float primitiveFloatProperty : primitiveFloatProperties)
			{
				this.primitiveFloatProperties.add(Float.valueOf(primitiveFloatProperty));
			}
		}
	}

	public void setPrimitiveFloatProperty(final float primitiveFloatProperty)
	{
		this.primitiveFloatProperty = primitiveFloatProperty;
	}

	public void setPrimitiveIntegerProperties(final int[] primitiveIntegerProperties)
	{
		this.primitiveIntegerProperties.clear();

		if (primitiveIntegerProperties != null)
		{
			for (final int primitiveIntegerProperty : primitiveIntegerProperties)
			{
				this.primitiveIntegerProperties.add(Integer.valueOf(primitiveIntegerProperty));
			}
		}
	}

	public void setPrimitiveIntegerProperty(final int primitiveIntegerProperty)
	{
		this.primitiveIntegerProperty = primitiveIntegerProperty;
	}

	public void setPrimitiveLongProperties(final long[] primitiveLongProperties)
	{
		this.primitiveLongProperties.clear();

		if (primitiveLongProperties != null)
		{
			for (final long primitiveLongProperty : primitiveLongProperties)
			{
				this.primitiveLongProperties.add(Long.valueOf(primitiveLongProperty));
			}
		}
	}

	public void setPrimitiveLongProperty(final long primitiveLongProperty)
	{
		this.primitiveLongProperty = primitiveLongProperty;
	}

	public void setPrimitiveShortProperties(final short[] primitiveShortProperties)
	{
		this.primitiveShortProperties.clear();

		if (primitiveShortProperties != null)
		{
			for (final short primitiveShortProperty : primitiveShortProperties)
			{
				this.primitiveShortProperties.add(Short.valueOf(primitiveShortProperty));
			}
		}
	}

	public void setPrimitiveShortProperty(final short primitiveShortProperty)
	{
		this.primitiveShortProperty = primitiveShortProperty;
	}

	public void setShortCollection(final Collection<Short> shortCollection)
	{
		this.shortCollection.clear();

		if (shortCollection != null)
		{
			this.shortCollection.addAll(shortCollection);
		}
	}

	public void setShortProperties(final Short[] shortProperties)
	{
		this.shortProperties.clear();

		if (shortProperties != null)
		{
			this.shortProperties.addAll(Arrays.asList(shortProperties));
		}
	}

	public void setShortProperty(final Short shortProperty)
	{
		this.shortProperty = shortProperty;
	}

	public void setStringCollection(final Collection<String> stringCollection)
	{
		this.stringCollection.clear();

		if (stringCollection != null)
		{
			this.stringCollection.addAll(stringCollection);
		}
	}

	public void setStringProperties(final String[] stringProperties)
	{
		this.stringProperties.clear();

		if (stringProperties != null)
		{
			this.stringProperties.addAll(Arrays.asList(stringProperties));
		}
	}

	public void setStringProperty(final String stringProperty)
	{
		this.stringProperty = stringProperty;
	}

	public void setUriCollection(final Collection<URI> uriCollection)
	{
		this.uriCollection.clear();

		if (uriCollection != null)
		{
			this.uriCollection.addAll(uriCollection);
		}
	}

	public void setUriProperties(final URI[] uriProperties)
	{
		this.uriProperties.clear();

		if (uriProperties != null)
		{
			this.uriProperties.addAll(Arrays.asList(uriProperties));
		}
	}

	public void setUriProperty(final URI uriProperty)
	{
		this.uriProperty = uriProperty;
	}

	public void setXmlLiteralProperty(final String xmlLiteralProperty)
	{
		this.xmlLiteralProperty = xmlLiteralProperty;
	}
}
