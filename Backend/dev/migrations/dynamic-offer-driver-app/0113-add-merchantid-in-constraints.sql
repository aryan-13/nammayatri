ALTER TABLE atlas_driver_offer_bpp.person drop CONSTRAINT person_unique_mobile_number_country_code;
ALTER TABLE atlas_driver_offer_bpp.person drop CONSTRAINT unique_email;
ALTER TABLE atlas_driver_offer_bpp.person drop CONSTRAINT unique_identifier;

ALTER TABLE atlas_driver_offer_bpp.person ADD CONSTRAINT person_unique_mobile_number_country_code UNIQUE (merchant_id, mobile_country_code, mobile_number_hash);
ALTER TABLE atlas_driver_offer_bpp.person ADD CONSTRAINT unique_email UNIQUE (merchant_id, email);
ALTER TABLE atlas_driver_offer_bpp.person ADD CONSTRAINT unique_identifier UNIQUE (merchant_id, identifier);

-- RIDER DETAILS
ALTER TABLE atlas_driver_offer_bpp.rider_details drop CONSTRAINT ride_details_unique_mobile_number;

ALTER TABLE atlas_driver_offer_bpp.rider_details ADD CONSTRAINT ride_details_unique_mobile_number UNIQUE (merchant_id, mobile_number_hash, mobile_country_code);
