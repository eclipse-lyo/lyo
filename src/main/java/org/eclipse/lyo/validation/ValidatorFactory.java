package org.eclipse.lyo.validation;

import org.eclipse.lyo.validation.impl.ShaclExValidatorImpl;

public final class ValidatorFactory {

    public static final Validator createShaclExValidator() {
        return new ShaclExValidatorImpl();
    }
}
