/*-*****************************************************************************
 * Copyright (c) 2017 Yash Khatri.
 *
 * All rights reserved. This program and the accompanying materials
 * are made available under the terms of the Eclipse Public License v1.0
 * and Eclipse Distribution License v. 1.0 which accompanies this distribution.
 *
 * The Eclipse Public License is available at http://www.eclipse.org/legal/epl-v10.html
 * and the Eclipse Distribution License is available at
 * http://www.eclipse.org/org/documents/edl-v10.php.
 *
 *
 * Contributors:
 *    Yash Khatri - initial API and implementation and/or initial documentation
 *******************************************************************************/

/**
 * @since 2.3.0
 */

package org.eclipse.lyo.validation.model;

import com.google.common.collect.ImmutableList;
import java.util.List;

/**
 * The Class ValidationResultModel.
 */
public class ValidationResultModel {

    /** The valid resources. */
    private List<ResourceModel> validResources;

    /** The invalid resources. */
    private List<ResourceModel> invalidResources;

    public ValidationResultModel() {
        super();
    }

    public ValidationResultModel(List<ResourceModel> validResources,
            List<ResourceModel> invalidResources) {
        super();
        this.validResources = validResources;
        this.invalidResources = invalidResources;
    }

    @Override
    public int hashCode() {
        final int prime = 31;
        int result = 1;
        result = prime * result + ((invalidResources == null) ? 0 : invalidResources.hashCode());
        result = prime * result + ((validResources == null) ? 0 : validResources.hashCode());
        return result;
    }

    @Override
    public boolean equals(Object obj) {
        if (this == obj) {
            return true;
        }
        if (obj == null) {
            return false;
        }
        if (getClass() != obj.getClass()) {
            return false;
        }
        ValidationResultModel other = (ValidationResultModel) obj;
        if (invalidResources == null) {
            if (other.invalidResources != null) {
                return false;
            }
        } else if (!invalidResources.equals(other.invalidResources)) {
            return false;
        }
        if (validResources == null) {
            if (other.validResources != null) {
                return false;
            }
        } else if (!validResources.equals(other.validResources)) {
            return false;
        }
        return true;
    }

    @Override
    public String toString() {
        return "ValidationResultModel [validResources=" + validResources + ", invalidResources=" + invalidResources + "]";
    }

    /**
     * Gets the total number of resources.
     *
     * @return the total number of resources
     */
    public long getTotalNumberOfResources() {
        return validResources.size() + invalidResources.size();
    }

    /**
     * Gets the valid resource count.
     *
     * @return the valid resource count
     */
    public long getValidResourceCount() {
        return validResources.size();
    }

    /**
     * Gets the invalid resource count.
     *
     * @return the invalid resource count
     */
    public long getInvalidResourceCount() {
        return invalidResources.size();
    }

    /**
     * Gets the valid resources.
     *
     * @return the valid resources
     */
    public List<ResourceModel> getValidResources() {
        return ImmutableList.copyOf(validResources);
    }

    /**
     * Sets the valid resources.
     *
     * @param validResources2 the new valid resources
     */
    public void setValidResources(List<ResourceModel> validResources2) {
        this.validResources = validResources2;
    }

    /**
     * Gets the invalid resources.
     *
     * @return the invalid resources
     */
    public List<ResourceModel> getInvalidResources() {
        return ImmutableList.copyOf(invalidResources);
    }

    /**
     * Sets the invalid resources.
     *
     * @param validResources2 the new invalid resources
     */
    public void setInvalidResources(List<ResourceModel> validResources2) {
        this.invalidResources = validResources2;
    }

}
