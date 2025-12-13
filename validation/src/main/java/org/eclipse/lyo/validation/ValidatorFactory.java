package org.eclipse.lyo.validation;

import org.eclipse.lyo.validation.impl.JenaShaclValidatorImpl;

public final class ValidatorFactory {

    /**
     * @deprecated Use {@link #createValidator()} instead.
     */
    @Deprecated
    public static final Validator createShaclExValidator() {
        return createValidator();
    }

    public static final Validator createValidator() {
        return new JenaShaclValidatorImpl();
    }
}
